{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK prune #-}

module Store.Sqlite where

import           CLI.Parser.Types               ( BoolExpr, interpretBoolExpr, boolExprToList )
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( join )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Aeson              hiding ( Result
                                                , Value
                                                )
import           Data.Bifunctor                 ( bimap )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Either
import           Data.Foldable                  ( foldl', traverse_ )
import           Data.Function                  ( (&) )
import           Helpers
import           Data.List                      ( isInfixOf, isPrefixOf, isSuffixOf )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Maybe as Maybe
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TL
import           Data.Time                      ( Day
                                                , UTCTime(..)
                                                , toGregorian
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Esqueleto
import qualified Database.Persist              as P
import qualified Database.Persist.Sqlite       as P
import           Database.Persist.Sqlite        ( BaseBackend
                                                , Entity(..)
                                                , Key(..)
                                                , SqlBackend
                                                , deleteWhere
                                                , get
                                                , insertKey
                                                , repsert
                                                , runMigration
                                                , runSqlite
                                                , selectList
                                                )
import           Database.Persist.TH
import           Debug.Trace                    ( trace )
import           GHC.Generics            hiding ( from )
import qualified Parse                         as P
import           Parse.Entry             hiding ( DefQueryVariant(..) )
import qualified Parse.Entry                   as P
import           Search
import           Store                          ( Result(..)
                                                , ToResult(..)
                                                )
import           Store.Sqlite.Types
import           Store.Types                    ( AttrTag(..) )
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )
import           Time
import           Web.PathPieces                 ( PathPiece(..) )


-- TODO (!!!) clean up source, remove revisions, document, reorder defs as necessary,
-- purge old or inapplicable TODOs.

---------------
--- STORAGE ---
---------------

-- newtype TS = TS UTCTime deriving Show
instance PathPiece UTCTime where
  fromPathPiece t = Nothing
  toPathPiece = T.pack . show

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ReadEntry
    Id UTCTime default=CURRENT_TIME
    title String
    author String
    deriving Show
DefEntry
    Id UTCTime default=CURRENT_TIME
    definitions TL.Text
    definitionTag DefTag
    attributionTag (Key ReadEntry) Maybe
    phraseOrDef PhraseOrDef
    pgNum Int Maybe
    pageTag PageTag Maybe
    deriving Show
QuoteEntry
    Id UTCTime default=CURRENT_TIME
    body String
    manualAttribution String Maybe
    attributionTag (Key ReadEntry) Maybe
    deriving Show
CommentaryEntry
    Id UTCTime default=CURRENT_TIME
    body String
    attributionTag (Key ReadEntry) Maybe
DialogueEntry
    Id UTCTime default=CURRENT_TIME
    body String
    attributionTag (Key ReadEntry) Maybe
PageNumberEntry
    Id UTCTime default=CURRENT_TIME
    pgNum Int
    pageTag PageTag
    attributionTag (Key ReadEntry) Maybe
DumpEntry
    Id UTCTime default=CURRENT_TIME
    dumps String
|]


data EntryType = ERead
               | EQuote
               | EDef
               | ECommentary
               | EDialogue
               | EPageNumber
               | EDump
               deriving (Eq, Show)

class Tagged a where
  getAttributionTag :: a -> Maybe (Key ReadEntry)
  getQuoteEntry :: a -> Maybe QuoteEntry

instance Tagged DefEntry where
  getAttributionTag = defEntryAttributionTag
  getQuoteEntry _ = Nothing

instance Tagged QuoteEntry where
  getAttributionTag = quoteEntryAttributionTag
  getQuoteEntry = return

instance Tagged CommentaryEntry where
  getAttributionTag = commentaryEntryAttributionTag
  getQuoteEntry _ = Nothing

instance Tagged DialogueEntry where
  getAttributionTag = dialogueEntryAttributionTag
  getQuoteEntry _ = Nothing

instance Tagged PageNumberEntry where
  getAttributionTag = pageNumberEntryAttributionTag
  getQuoteEntry _ = Nothing

-- Note that `headswords` is JSON text containing either: 
--  * a headword list: { "headwords" : [ <word>, .. ] }; or
--  * an inline definition/comparison: 
--        { "inlineDefinitioPns" : [ { "headword" : <word> , "meaning" : <meaning>}, .. ] }
--   - a headword list:  { "headword" : <word>, "meaning": <meaning> }, .. ]; or
--   - a comparison: 
--
-- | An 'InlineDef' is a haskell representation of the JSON-encoded
-- 'definitions' column of the 'DefEntry' table; that is, the string in an
-- 'Entity DefEntry's "definitions" field decodes to an 'InlineDef'.
data InlineDef = InlineDef
  { headword :: Text
  , meaning :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON InlineDef where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON InlineDef

-- TODO speed up @--def-search@ (everything else is blazing fast)
--    
--    Why, you might ask? Well because we want muse to snap, and >1.5ms for a
--    simple definition meaning search doesn't cut it. Luckily, the select and
--    apply predicates all in one pass optimized (at least a little) by
--    sqlite's query planner tactic has worked, and I mean /spectacularly/: a
--    quote body search which previously took 0.12ms now takes a mean 0.01ms!
--    The problem is all the goddamn marshalling between sqlite's json-bearing
--    'DefEntry's and its unpaccked haskell counterpart 'DefEntry''. To avoid
--    this we could either:
--
--      i.   make a sparse definition table with fields sufficient for a
--      comparision, in many cases leaving most null;
--
--      ii.  create a table for each definition variant; or,
--
--      iii. ( TODO hash this out) convert multi-entries, like comparisons,
--      into mutually referencing inline defs, all headword lists each into
--      individual entries, each of which converted entry component wuold no
--      longer have a timestamp as a primary key but some auto-incremented
--      internal integer. 
--
--    Proposed DefEntry schema under option (i):
--      
--      A sparsely populated row for each definition entry would need fields to
--      support: 
--        * headword lists
--        * '--- vs ---'-separated inline definitions.
--
--        THIS WON'T ALLOW MORE CONVOLUTED SEARCH PREDICATES.
--

--  Lists of definition variants are difficult to model sqlite. We have two
--  options:
--    i. json-encode lists and apply (only complex, as a potential
--       optimization) predicates to haskell lists
--
--    ii. Unpack variable length arrays into sql tables referenced by a table
--        keyed by timestamps. In the case of the DefEntry, we would have
--        a...WAIT
--
--  (!!LATEST!!) we can speed up a /lot/ of queries by apply, e.g., string searches
--  with SQL's LIKE operator, and then apply any remaining specifications of
--  the query to only the remainder of the entries; in this way, we can avoid
--  decoding /most/ of the unwanted entries and only suffer the time overhead
--  for a minority of partial matches.

--
--
--
--
--      
--
-- TODO use CASE statements to short-circuit def search select queries when the
-- current entity has the wrong variant tag--left-to-right evaluation, and
-- short-circuiting of boolean expressions I don't think is guaranteed.
--
data DefEntry' = Headwords [TL.Text] 
               | Inlines [InlineDef] 
               deriving (Eq, Show, Generic)

instance ToJSON DefEntry' where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefEntry'

-- | JSON-encodable 'DumpEntry'. A 'DumpEntry'' is a list of 'Dump's belonging
-- to a single 'Day' (its primary key).
newtype DumpEntry' = DumpEntry'
  { dumps :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DumpEntry' where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DumpEntry'

type DB m a = ReaderT SqlBackend m a

fromPageNum :: P.PageNum -> (PageTag, Int)
fromPageNum pageNum = case pageNum of
  (P.Page    pg) -> (Page', fromIntegral pg)
  (P.PStart  pg) -> (PStart', fromIntegral pg)
  (P.PEnd    pg) -> (PEnd', fromIntegral pg)
  (P.PFinish pg) -> (PFinish', fromIntegral pg)

-- | Converts an 'AttrTag' into a primary key of the 'ReadEntry' table; that
-- is, a wrapped 'UTCTime'.
fromAttrTag :: AttrTag -> Key ReadEntry
fromAttrTag = ReadEntryKey . attrId

-- | Writes and tags a 'Day'\'s 'LogEntry's to the database. A singly indented
-- entry is tagged with the id of the top-level parent the least lines above
-- it; if no such parent exists the entry is left untagged and a (TODO) warning
-- is logged.
--
-- TODO (!!!!) protect against (accidental) duplicate timestamps--they result
-- in a primary key collision. !! Primary key collisions are a fatal error. The
-- only issue within this function (and 'writeLogEntry') is that the error
-- message is vague. As such, we will catch the exception here, inspect the
-- primary keys, and return a more informative error message. However, this
-- should /never/ happen again, as the parser is wholly responsible for
-- squawking at ill-formed input. Fix the damn parser.
--
-- TODO works in 'demo': convert back to  @[LogEntry]@ to test that
-- @(writeEntry .  readRead) == id@ is true for any entry and that attributions
-- are correctly determined (which requires manual tagging).
writeDay :: MonadIO m => Day -> [LogEntry] -> DB m [Either String UTCTime]
writeDay day
  = let
      go
        :: MonadIO m
        => Maybe AttrTag
        -> [LogEntry]
        -> DB m [Either String UTCTime]
      go _ [] = return []
      go mTag (h : t)
        |
        -- a. toplevel and read entry -> insert, create tag from id and place in (optional) accumulator, recurse
          isTopLevel h && isRead h
        = do
          eitherId <- writeLogEntry day h Nothing
          case eitherId of
            Left err ->
              ((Left
               $ "WARNING: writeDay: found entry that is both Read and Dump\ncont.: "
               ++ err
               ) :
                )
                <$> go Nothing t
            -- begin traversal with readEntryKey in accumulator (note that it
            -- should be immediately discarded if the next entry is not the
            -- child of the current read entry).
            Right readEntryKey -> go (Just $ AttrTag readEntryKey) t
        | 
                 -- WARNING: this is impossible: it means that the entry is a Read
                 -- variant and a Dump variant, so if it occurs, it will have
                 -- been by programmer error. As such, we simply log the
                 -- irregularity, 
        -- b. toplevel and not read entry ->  insert without tag, recurse with accumulator := Nothing 
          isTopLevel h
          -- discards accumulator should the last entry have been of the Read
          -- variant
        = (:) <$> writeLogEntry day h Nothing <*> go Nothing t
        |
        -- d. indented one level or more, read entry -> 
        --      insert untagged, log warning describing weird nested readings (now 
        --      this may warrant future support, but since we're only replicating 
        --      the existing behavior of muse, this is out of scope.), and pass
        --      previous accumulator through.
          isIndentedTo 1 h && isRead h
        = (\h h' t -> h : h' : t)
          <$> return
                (Left
                  "WARNING: writeDay: found indented read entry, leaving untagged"
                )
          <*> writeLogEntry day h Nothing -- NOTE: leaving read entries untagged
          <*> go mTag t
        |
        -- c. indented one level or more, not read entry ->
        --      i.  accumulator is Nothing -> insert indented untagged, recurse
        --      with previous accumulator
        --      ii. accumulator is (Just attrTag) -> insert tagged, recurse
        --      with previous accumulator
          isIndentedTo 1 h
        = (:) <$> writeLogEntry day h mTag <*> go mTag t
        |
        -- otherwise insert untagged, recurse with accumulator := Nothing
          otherwise
        = (:) <$> writeLogEntry day h Nothing <*> go Nothing t
    in
      go Nothing

-- | A write action for a single 'LogEntry'. It's intended for application to
-- lists of log entries grouped by day: for each day's list of log entries,
-- 'writeEntry' is partially applied to the 'Day' and mapped over the entries.
--
-- If a given key exists it will be overwritten. This is excusable as the
-- parser excludes duplicate keys.
writeLogEntry
  :: MonadIO m
  => Day
  -> LogEntry
  -> Maybe AttrTag
  -> DB m (Either String UTCTime)
writeLogEntry day logEntry mAttrTag =
  -- trace ("writeLgEntry: " <> show mAttrTag <> "\n  logEntry:" <> show logEntry) $ 
    case logEntry of
        Dump dumpContents ->
          return $ Left "Store.Sqlite.writeEntry: Dump handling not implemented"
          -- FIXME add 'DumpEntry' type & table
          --
          --  with acid-state, we kept a list of dumps for each day. In order to
          --  the same with sqlite and stay consistent with our JSON-based list
          --  workaround, we will store a list of 'DumpEntry's as JSON, of the form
          --  [ { "dumpBody" : <string>}, .. ].
          --
        TabTsEntry (indentation, timestamp, entry)
          -> let utc = toUTC day timestamp
             in
               (case entry of
                   Def dq -> repsert (DefEntryKey utc) $ toDefEntry mAttrTag dq
                   Read title author ->
                     repsert (ReadEntryKey utc) $ ReadEntry title author
                                 -- FIXME relies on empty attribution string when manual
                                 -- attribution is omitted
                   Quotation quoteBody manualAttribution mPgNum ->
                     repsert (QuoteEntryKey utc) $ QuoteEntry
                       quoteBody
                       (if manualAttribution == ""
                         then Nothing
                         else Just manualAttribution
                       )
                       (fromAttrTag <$> mAttrTag)
                   Commentary commentBody ->
                     repsert (CommentaryEntryKey utc)
                       $   CommentaryEntry commentBody
                       $   fromAttrTag
                       <$> mAttrTag
                   PN pageNum ->
                     let (pageTag, pgNum) = fromPageNum pageNum
                     in
                       -- trace (show pageTag <> ", " <> show pgNum <> ", " <> show utc) $ 
                         repsert (PageNumberEntryKey utc) $ PageNumberEntry pgNum pageTag (fromAttrTag <$> mAttrTag)
                   Phr phrase ->
                     repsert (DefEntryKey utc) $ phraseToDefEntry mAttrTag phrase
                   Dialogue dialogueBody ->
                     repsert (DialogueEntryKey utc)
                       $   DialogueEntry dialogueBody
                       $   fromAttrTag
                       <$> mAttrTag
                   Parse.Entry.Null -> return ()
                 )
                 >> return (Right utc)

main :: IO ()
main = runSqlite "test.db" $ do
  runMigration migrateAll
  -- read entry
  now <- liftIO getCurrentTime
  let wutheringHeightsId = ReadEntryKey now
  insertKey wutheringHeightsId $ ReadEntry "Wuthering Heights" "Emily Bronté"
  -- def entry
  now' <- liftIO getCurrentTime
  let inline = TL.decodeUtf8 $ encode $ Inlines
        [InlineDef "mizzle" "a misty drizzle"]
      defEntryId = DefEntryKey now'
  insertKey defEntryId $ DefEntry inline
                                  Inline'
                                  (Just wutheringHeightsId)
                                  Definition'
                                  Nothing
                                  Nothing
  (allDefs :: [Entity DefEntry]  ) <- selectList [] []
  (allReads :: [Entity ReadEntry]) <- selectList [] []
  liftIO $ BLC.putStrLn $ encode $ Inlines
    [InlineDef "mizzle" "a misty drizzle"]
  let entry :: Either String Entry
      entry = toEntry $ DefEntry
        (TL.decodeUtf8 $ encode $ Inlines [InlineDef "mizzle" "a misty drizzle"]
        )
        Inline'
        Nothing
        Definition'
        Nothing
        Nothing
  liftIO $ putStr "entry: " >> pPrint entry
  liftIO
    $ pPrint
    $ let xs :: [Either String Entry]
          xs = fmap (toEntry . entityVal) allDefs
      in  rights xs
  liftIO $ pPrint $ fmap entityVal allReads
  -- clean up 
  deleteWhere [DefEntryId P.==. defEntryId]
  deleteWhere [ReadEntryId P.==. wutheringHeightsId]
  --runSqlite ":memory:" $ do
    --oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    --liftIO $ print (oneJohnPost :: [Entitygggg BlogPost])
    --john <- get johnId
    --liftIO $ print (john :: Maybe Person)
    --delete janeId
    --deleteWhere [BlogPostAuthorId ==. johnId]

-- # DB schema TODO list:
-- ## Write
-- □  ReadEntry
--    □  convert
--    □  write to db
-- □  QuoteEntry
--    □  convert
--    □  write to db
-- □  DefEntry
--    □  convert
--    □  write to db
-- □  CommentEntry
--    □  convert
--    □  write to db
-- □  DialogueEntry
--    □  convert
--    □  write to db
-- □  DumpEntry
--    □  convert
--    □  write to db
-- □  PhraseEntry
--    □  convert
--    □  write to db
-- ## Read
-- □  read from db types to internal format  
--
schemaTODO = undefined

entryToResult
  :: ToEntry String record => UTCTime -> record -> Either String Result
entryToResult utc = fmap (TsR utc) . toEntry 

class Show err =>
      ToEntry err a where
  toEntry :: a -> Either err Entry

instance ToEntry String DefEntry where
  toEntry DefEntry {..} -- Right $ Quotation "" "" Nothing
   =
    case eitherDecode $ TL.encodeUtf8 defEntryDefinitions of
      Right defTag ->
        case defEntryPhraseOrDef of
          Definition' ->
            case defTag of
              Headwords hws -> Right $ Def $ P.Defn Nothing $ TL.unpack <$> hws
              Inlines inlineDefs ->
                case inlineDefs of
                  [] ->
                    Left "toEntry: definition: found empty list of inline defs"
                  [InlineDef {..}] ->
                    Right $
                    Def $ P.InlineDef (T.unpack headword) (T.unpack meaning)
                  [h, t] ->
                    Right $
                    Def $
                    P.DefVersus
                      (T.unpack $ headword h)
                      (T.unpack $ meaning h)
                      (T.unpack $ headword t)
                      (T.unpack $ meaning t)
                  _ ->
                    Left
                      "toEntry: definition: found more than two inline defs, corrupt input."
          Phrase' ->
            case defTag of
              Headwords hws -> Right $ Phr $ Plural $ TL.unpack <$> hws
              Inlines inlineDefs ->
                case inlineDefs of
                  [] -> Left "toEntry: phrase: found empty list of inline defs"
                  [InlineDef {..}] ->
                    Right $ Phr $ Defined (T.unpack headword) (T.unpack meaning)
                  _ ->
                    Left
                      "toEntry: phrase: found more than two inline defs, corrupt input."
      Left err -> Left err

instance ToEntry String ReadEntry where
  toEntry ReadEntry {..} = pure $ Read readEntryTitle readEntryAuthor

instance ToEntry String QuoteEntry where
  toEntry QuoteEntry {..} =
    pure $
    Quotation quoteEntryBody (fromMaybe "" quoteEntryManualAttribution) Nothing -- FIXME PageNum support

instance ToEntry String CommentaryEntry where
  toEntry = pure . Commentary . commentaryEntryBody -- FIXME attribution discarded

instance ToEntry String DialogueEntry where
  toEntry = pure . Dialogue . dialogueEntryBody -- FIXME attribution discarded

instance ToEntry String PageNumberEntry where
  toEntry PageNumberEntry {..} =
    let pageNumVariant =
          case pageNumberEntryPageTag of
            Page' -> P.Page
            PStart' -> P.PStart
            PEnd' -> P.PEnd
            PFinish' -> P.PFinish
    in pure $ PN $ pageNumVariant $ fromIntegral pageNumberEntryPgNum

-- | Create SQL-writeable 'DefEntry' from a 'DefQuery`.
--
-- Note: in the future, the parser may directly marshal to the more db-friendly
-- format; for the present, however, we mourn and carry on as best we know how.
toDefEntry :: Maybe AttrTag -> P.DefQuery -> DefEntry
toDefEntry mAttrTag dq = case dq of
  P.Defn mPg hws -> DefEntry
    (TL.decodeUtf8 $ encode $ Headwords $ TL.pack <$> hws)
    Headwords'
    (fromAttrTag <$> mAttrTag)
    Definition'
    (fromIntegral <$> mPg)
    Nothing
  P.InlineDef headword meaning -> DefEntry
    (TL.decodeUtf8 $ encode $ Inlines
      [InlineDef (T.pack headword) (T.pack meaning)]
    )
    Inline'
    (fromAttrTag <$> mAttrTag)
    Definition'
      -- TODO add page nums (prefix types?) for all definition/phrase types.
    Nothing
    Nothing
  P.DefVersus headword meaning headword' meaning' -> DefEntry
    (TL.decodeUtf8 $ encode $ Inlines
      [ InlineDef (T.pack headword)  (T.pack meaning)
      , InlineDef (T.pack headword') (T.pack meaning')
      ]
    )
    Comparison'
    (fromAttrTag <$> mAttrTag)
    Definition'
      -- TODO add page nums (prefix types?) for all definition/phrase types.
    Nothing
    Nothing

-- | Like 'toDefEntry' but converts from 
phraseToDefEntry :: Maybe AttrTag -> Phrase -> DefEntry
phraseToDefEntry mAttrTag phrase = case phrase of
  Plural hws -> DefEntry
    (TL.decodeUtf8 $ encode $ Headwords $ TL.pack <$> hws)
    Headwords'
    (fromAttrTag <$> mAttrTag)
    Phrase'
    Nothing
    Nothing
  Defined headword meaning -> DefEntry
    (TL.decodeUtf8 $ encode $ Inlines
      [InlineDef (T.pack headword) (T.pack meaning)]
    )
    Inline'
    (fromAttrTag <$> mAttrTag)
    Phrase'
    -- TODO add page nums (prefix types?) for all definition/phrase types.
    Nothing
    Nothing

--------------
--- SEARCH ---
--------------

-- | TODO
--
-- □  replace strings with text wherever possible--I have in mind the search
-- strings.
--
searchTodo = undefined

type Author = String
type Title  = String

-- | Search preferences (extracted from command line arguments).
--
-- Each @check*@ field signals whether the corresponding entry variant is to be
-- included in the final output.
--
-- The @*Search@ fields contain variant specific search configuration. As yet,
-- all but 'DefSearch' are type aliases for @[String -> Bool]@.
-- 
data SearchConfig = SearchConfig
  { since :: Day
  , before :: Day
  , checkDumps :: Bool
  , checkQuotes :: Bool
  , checkDialogues :: Bool
  , checkComments :: Bool
  , checkDefinitions :: Bool
  , attributionPreds :: ([String], [String]) -- tuple of search author, title search strings
  , definitionSearch :: DefSearchR

  , quoteSearch :: QuotationSearch
  , commentarySearch :: CommentarySearch
  , dialogueSearch :: DialogueSearch
  , dumpSearch :: [String]
  } deriving Show


-- note on cli @--def@/@-d@ should take a string of the format
-- @"<headword>:<meaning>"@ where either headword or meaning can be omitted and
-- the colon will inform the search dispatch whether the search string applies
-- to headwords or meanings--e.g., @":compensation"@ would search for meanings
-- containing "compensation"; @"malignant"@ and @"malignant:"@ would search for
-- headwords matching "malignant"; and so on. The bare invocation (without an
-- argument), @"-d"@/@"--def"@, would select all definitions.


-- | Dispatch search.
--
-- The new CLI will make it so that whenever a variant-specific flag is passed,
-- e.g., a definition headword search string, only definitions of the
-- appropriate type are included in the output /unless/ another
-- variant-specific flag is given in which case the two searches will be
-- performed separately, the results of which will be concatenated--that is,
-- assuming that the variants specified are /different/; if they are the same,
-- one narrower search will be performed.
--
-- When there are /no/ predicates save those on date or attribution, all
-- entries within that range are returned. Otherwise, only entries matching the
-- given predicates are included--that is, entries to which predicates are not
-- applicable are excluded.
--
-- Note that dumps are as yet excluded.
-- 
-- Note also that def variants should be generated by the CLI's parser.
dispatchSearch
  :: MonadIO m => String -> SearchConfig -> DB m [Either String Result]
dispatchSearch logPath SearchConfig {..}
  = let -- x :: Int 
      defSearch = definitionSearch -- if null (defVariants definitionSearch) then definitionSearch { defVariants = allDefVariants } else definitionSearch
      (authPreds, titlePreds) = attributionPreds
        --      search :: _
      search =
        if null dialogueSearch
           && not checkDialogues
           && null commentarySearch
           && not checkComments
           && null quoteSearch
           && not checkQuotes
           && not checkDefinitions
           && isDefSearchNullR definitionSearch -- test for null before injecting 'allDefVariants'
        then -- show all in date range matching attribution requirements
          join <$> sequence
            [ -- replaces 'DefSearch' with 'allDefVariants'.
              filterDefs' since before authPreds titlePreds defSearch { defVariantsR = allDefVariants }
            , filterQuotes' since before authPreds titlePreds quoteSearch
            , filterCommentaries' since
                                  before
                                  authPreds
                                  titlePreds
                                  commentarySearch
            , filterDialogues since before authPreds titlePreds dialogueSearch
            ]
        else
          join <$> sequence
            [ if not (null quoteSearch) || checkQuotes
              then filterQuotes' since before authPreds titlePreds quoteSearch
              else return []
            , if not (null commentarySearch) || checkComments
              then filterCommentaries' since
                                       before
                                       authPreds
                                       titlePreds
                                       commentarySearch
              else return []
            , if not (isDefSearchNullR defSearch) || checkDefinitions
              then filterDefs' since before authPreds titlePreds 
                  -- when the definition flag is passed, but no def variants
                  -- are specified, return all variants; otherwise, use the
                  -- defSearch unmodified.
                  (if null (defVariantsR defSearch) && checkDefinitions
                      then defSearch { defVariantsR = allDefVariants }
                      else defSearch)
              else return []
            ]
    in
      search


-- | Attribution filters: if the title and author infix lists are empty, these
-- are skipped over, otherwise they are the first in the chain of filters.
-- Eventually, we may refactor the filters into a single pass, should
-- performance become an issue.
--
-- Each record type/table/entry type will have its own custom filtration
-- pipeline: the first segment of each will be a switch to include or exclude
-- that entry type in the final list of (filtered) entries.
--
-- For instance, 'DefEntry`s will pass through the following filters in order:
--    1. entry type inclusion/exclusion--this dispatch takes in the CLI options
--    and dispatches the requested entry-variant filters; 
--    3. date filtration (based on query's date range)--see 'withinDateRange';
--    3. read attribution--see 'applyReadPreds'; and
--    4. definition variant :
--        i. run 'withinDateRange' then 'applyReadPreds'
--        ii. apply entry-variant specific filters, e.g.,
--          * phrase | definition
--          * headwords | inline | comparision (a.t.m. implies the definition
--          filter).
--
--  More generally, the pipeline reduces to steps 1-3 as above and then some
--  entry variant specific filtration based on its unique fields. Note that
--  when we receive a query containing a predicate on a field not common to all
--  entry variants, only variants that have the field will be included in the
--  output (however in the first pass we should imitate the current
--  functionality as a kind of regression testing to see whether the new
--  pipeline has all the functionality of the old).
attributionSatisfies
  :: MonadIO m => [String] -> [String] -> Key ReadEntry -> DB m Bool
attributionSatisfies authorPreds titlePreds readKey = do
  maybeReadEntry <- get readKey
  return $
    -- if the entry has no read attribution and we are given author or
    -- title predicates, then the entry is omitted from the filtered
    -- output.
           case maybeReadEntry of
    Just ReadEntry {..} ->
      and (isInfixOf <$> authorPreds <*> pure readEntryAuthor)
        && and (isInfixOf <$> titlePreds <*> pure readEntryTitle)
    Nothing -> False

-- | Applies auth/title predicates to a quote's manual attribution. If no
-- manual attribution is present, @Nothing@ is returned.
manualAttributionSatisfies :: [String] -> [String] -> QuoteEntry -> Maybe Bool
manualAttributionSatisfies authorPreds titlePreds QuoteEntry { quoteEntryManualAttribution }
  = do
    attr <- quoteEntryManualAttribution
    return $ and (isInfixOf <$> authorPreds <*> pure attr) && and
      (isInfixOf <$> titlePreds <*> pure attr)

-- | Checks whether a single 'Entry'\'s title/author attribution satisfies the
-- given search strings.
taggedEntrySatisfies
  :: (MonadIO m, Tagged record)
  => [String]
  -> [String]
  -> Entity record
  -> DB m (Maybe (Entity record))
taggedEntrySatisfies authorPreds titlePreds entity@Entity {..} =
  case getAttributionTag entityVal of
    Just readKey -> do
      -- FIXME when an entry has an inferred attribution, by indentation, /and/
      -- a manual attribution, it satisfies the search--that is, it becomes a
      -- result--if the search predicates succeed for either attribution.
      satisfies <- attributionSatisfies authorPreds titlePreds readKey
      let manualSatisfies =
            fromMaybe True
              $   getQuoteEntry entityVal
              >>= manualAttributionSatisfies authorPreds titlePreds
      return $ if satisfies || manualSatisfies then Just entity else Nothing
    Nothing -> return Nothing

-- | Title and author search predicates test whether the given search strings
-- are all infixes of the title or author, respectively.
--
---- FIXME too many passes (Q: how to know whether fusion has been applied?  Check core?)
applyReadPreds
  :: (MonadIO m, Tagged record)
  => [String] --  Author predicates
  -> [String]
  -> [Entity record]
  -> DB m [Entity record]
applyReadPreds authorPreds titlePreds entities =
  fmap catMaybes
    $ sequence
    $ -- trace ("applyReadPreds: \n" <> "# results: " <> show (length x)) $ 
      taggedEntrySatisfies authorPreds titlePreds <$> entities
      

-- | Collects entries of from a single variant's table that were entered within
-- the given inlusive date range. This amounts to a select query with
-- constraints on the primary key.
--
-- Having determined which entry variants are desired by the user, this filter
-- is the first segment in each variant-specific pipeline. It's passed the
-- 'EntryType' associated with the variant table it's to select from.
withinDateRange
  :: (MonadIO m, PersistEntityBackend record ~ SqlBackend, PersistEntity record)
  => EntityField record (Key record)
  -> (UTCTime -> Key record)
  -> Day
  -> Day
  -> DB m [Entity record]
withinDateRange entryIdType keyWrapper sinceDay beforeDay =
  let
    since  = dayToUTC sinceDay
    before = dayToUTC beforeDay
    dateConstraints =
      [entryIdType P.>=. keyWrapper since, entryIdType P.<=. keyWrapper before]
  in
    selectList dateConstraints []

--- QUERY HELPERS

-- | Esqueleto constraint to select entries within a given date range.
dateRangeConstraint
  :: PersistEntity record
  => EntityField record (Key record)
  -> (UTCTime -> Key record)
  -> SqlExpr (Entity record)
  -> Day
  -> Day
  -> SqlExpr (Value Bool)
dateRangeConstraint idType keyWrapper entityUTC sinceDay beforeDay =
  let since  = dayToUTC sinceDay
      before = dayToUTC beforeDay
  in  (entityUTC ^. idType >=. val (keyWrapper since))
        &&. (entityUTC ^. idType <=. val (keyWrapper before))


-- | An esqueleto constraint that checks whether a string is @LIKE@ all of a
-- list of search strings, the syntax for which is specified by the Sqlite docs
-- for @LIKE@.
attributionConstraint
  ::  -- Esqueleto query expr backend => 
     SqlExpr (Value (Maybe String)) -> [String] -> SqlExpr (Value Bool)
attributionConstraint bodyStr = foldr
  (\searchStr rest -> (bodyStr `like` just (val searchStr)) &&. rest)
  (val True)

-- | Applies both 
attributionConstraints
  :: SqlExpr (Maybe (Entity ReadEntry))
  -> [String]
  -> [String]
  -> SqlExpr (Value Bool)
attributionConstraints readEntry authPreds titlePreds =
  foldr
      (\authStr rest ->
        (readEntry ?. ReadEntryAuthor `like` just (val authStr)) &&. rest
      )
      (val True)
      authPreds
    &&. foldr
          (\titleStr rest ->
            (readEntry ?. ReadEntryTitle `like` just (val titleStr)) &&. rest
          )
          (val True)
          titlePreds



--- DEFINITION SEARCH 

-- | Search predicates specifically for definitions. See 'filterDef'\'s
-- documentation for more details.

data DefSearch = DefSearch
  { defVariants :: [P.DefQueryVariant]
  , headwordPreds :: [String -> Bool]
  , meaningPreds :: [String -> Bool] }


data StrSearch s = PrefixSearch s
                      | InfixSearch s
                      | SuffixSearch s
                      deriving (Eq, Show)

instance Functor StrSearch where
  fmap f (PrefixSearch s) = PrefixSearch (f s)
  fmap f (InfixSearch s) = InfixSearch (f s)
  fmap f (SuffixSearch s) = SuffixSearch (f s)

fromStrSearch :: StrSearch s -> s
fromStrSearch (PrefixSearch s) = s
fromStrSearch (InfixSearch s) = s
fromStrSearch (SuffixSearch s) = s

sqlPadStrSearch :: StrSearch String -> String
sqlPadStrSearch (PrefixSearch s) = s++"%"
sqlPadStrSearch (InfixSearch s) = '%':s++"%"
sqlPadStrSearch (SuffixSearch s) = '%':s



strSearchToPredicate :: StrSearch String -> (String -> Bool)
strSearchToPredicate (PrefixSearch s) = isPrefixOf s
strSearchToPredicate (InfixSearch s) = isInfixOf s
strSearchToPredicate (SuffixSearch s) = isSuffixOf s

-- | When theere is no 'BoolExpr', return True (this result is usually ANDed
-- and so we don't wish to declare an entry unsatisfactory when there is no
-- search expression. 
applyBoolExpr :: Maybe (BoolExpr (StrSearch String)) -> String -> Bool
applyBoolExpr Nothing   _ = True
applyBoolExpr (Just be) s = interpretBoolExpr ((s &) . strSearchToPredicate) be



-- Revision of 'DefSearch' to accomodate deferred string search type dispatch
-- (prefix vs infix vs suffix).
data DefSearchR = DefSearchR
  { defVariantsR :: [P.DefQueryVariant]
  , headwordPredsR :: Maybe (BoolExpr (StrSearch String))
  , meaningPredsR :: Maybe (BoolExpr (StrSearch String)) }
  deriving Show

instance Show DefSearch where
  show (DefSearch variants hws mns) =
    "DefSearch " <> show variants <> " " <> show (length hws) <> " " <>
    show (length mns)



isDefSearchNull :: DefSearch -> Bool
isDefSearchNull DefSearch {..} = null defVariants && null headwordPreds && null meaningPreds
 
isDefSearchNullR :: DefSearchR -> Bool
isDefSearchNullR DefSearchR {..} =
  null defVariantsR
    && Maybe.isNothing headwordPredsR
    && Maybe.isNothing meaningPredsR


defSearchConstraint
  :: SqlExpr (Value String) -> [String] -> SqlExpr (Value Bool)
defSearchConstraint  body = foldr
  (\searchStr rest -> (body `like` val searchStr) &&. rest)
  (val True)


-- | Runs 'filterDef' on the results of 'selectDefs'.
--
-- If 'DefSearch' contains no definition variants, then all variants will be
-- included in the output.
filterDefs
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> DefSearchR
  -> DB m [Either String Result]
filterDefs since before authPreds titlePreds defSearch = do
  defEntries <- selectDefs since before authPreds titlePreds
  return
    $   filtermap
          (\e -> case e of
            Left  err -> Just (Left err)
            Right mr  -> do
              r <- mr
              Just (Right r)
          )
    $   filterDefR defSearch -- this costly af
    <$> defEntries

-- | Since 'DefEntry's need to be unpacked before the 'DefSearch' can be
-- applied, the filter entails first selecting all 'DefEntry's within the
-- requested date range and satisfying the any (optionally) present attribution
-- predicates, and secondly applying the 'DefSearch' variant and
-- headword/meaning search predicates.
--
--  If /any/ 'DefEntry' json is found to be malformed the incident will be
--  logged to ~/.muse/logs/. Assumes that the log path is /not/ suffixed with a
--  forwardslash.

selectDefs
  :: MonadIO m => Day -> Day -> [String] -> [String] -> DB m [Entity DefEntry]
selectDefs since before authPreds titlePreds = do
  let dateConstraint defEntry =
        dateRangeConstraint DefEntryId DefEntryKey defEntry since before
  if null authPreds && null titlePreds
    then select $ from $ \defEntry -> do
      where_ $ dateConstraint defEntry
      return defEntry
    else select $ from $ \(defEntry `LeftOuterJoin` readEntry) -> do
      on
        (defEntry ^. DefEntryAttributionTag ==. just (readEntry ^. ReadEntryId))
      where_
        $ attributionConstraint (just $ readEntry ^. ReadEntryAuthor) authPreds
        &&. attributionConstraint (just $ readEntry ^. ReadEntryTitle)
                                  titlePreds
        &&. dateConstraint defEntry
      return defEntry


-- TODO fix TL.Text <-> string conversions
-- FIXME (optimization refactor) search json for tags to narrow down candidate
-- entries, for e.g., selecting all DefVersus entries, (which still takse
-- >140ms on average).
filterDefs'
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> DefSearchR
  -> DB m [Either String Result]
filterDefs' since before authPreds titlePreds defSearch = do
  let mns = maybe [] (foldr ((:) . sqlPadStrSearch) [] ) (meaningPredsR defSearch)
      -- this extracts all the string searches and pads them as infix searches
      -- for use with SQL's LIKE operator.
      infixSearches = maybe mns (foldr ((:) . sqlPadStrSearch) mns) (headwordPredsR defSearch)
  -- liftIO $ traverse print infixSearches
  defEntries <- selectDefs' since before authPreds titlePreds infixSearches (defVariantsR defSearch) -- collect all strings
  return
    $   filtermap
          (\e -> case e of
            Left  err -> Just (Left err)
            Right mr  -> do
              r <- mr
              Just (Right r)
          )
    $   filterDefR defSearch -- this costly af
    <$> defEntries

-- | FIXME (REMOVE ME) janky workaround until completion of tag refactor
--
-- Note that a defversus filter now takes only around 40ms--for some reason
-- this is not nearly as good an improvement as expected (sql filters for,
-- e.g., meaning searches speed, which take only around 15ms). I think this is
-- due to that the query requested /all/ comparisons whereas the meaning
-- searches only requested matching entries, causing the former to spend extra
-- time marshalling data from sql to json in haskell to the final haskell
-- types. YES! adding a meaning predicate brought down the mean time to ~18ms.
--
-- Let's just ignore phrases to explore the performance gains of this approach.
--
jankyConvertDefQueryVariantToDefEntryTags :: P.DefQueryVariant -> DefTag
jankyConvertDefQueryVariantToDefEntryTags P.Phrase' = Inline' -- inline or defn? unaccountable behavior; we've chosen to (temporarily) weather the false positives
jankyConvertDefQueryVariantToDefEntryTags P.Defn' = Headwords'
jankyConvertDefQueryVariantToDefEntryTags P.InlineDef' = Inline'
jankyConvertDefQueryVariantToDefEntryTags P.DefVersus' = Comparison'


-- | Revision of 'selectDefs'' that braves false positives by applying each
-- string search predicates, viz., headword, meaning, to the json-encoded
-- definition entry. 
--
-- This will eliminate many obviously undesirable candidate
-- entries.
--
-- FIXME this relies on search strings being infix.
-- When we get around to implementing prefix and suffix searches, perhaps the
-- CLI's input structure could yield values of the following enum:
--
--   @
--   data SearchString = PrefixSearch String
--                     | InfixSearch String
--                     | SuffixSearch String
--                     deriving (Eq, Generic, Show)
--   @
-- 
-- And provide an sql-aware function pad the query appropriately; that is,
-- surround with "%" for infix, prefix "%" for prefix, and suffix "%" for
-- suffix. With this deferred search couching, 'selectDefs'' retains acesss to
-- the raw query with which it can apply in infix fashion to each def entry's
-- JSON representation. 
--
-- 'selectDefs'' takes as list of search strings that will be dispatched as
-- infix queries.
--
-- TODO we can also filter def subvariants here, as those are exposed as a field in
-- the sql row .
--
selectDefs'
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> [String]
  -> [P.DefQueryVariant]
  -> DB m [Entity DefEntry]
selectDefs' since before authPreds titlePreds infixSearches' defVariants = do
  let infixSearches = TL.pack <$> infixSearches'
  let querysDefTags = jankyConvertDefQueryVariantToDefEntryTags <$> defVariants
  let dateConstraint defEntry =
        dateRangeConstraint DefEntryId DefEntryKey defEntry since before
      matchesInfixSearches defEntry =
        genericSearchConstraint (defEntry ^. DefEntryDefinitions) infixSearches

  if null authPreds && null titlePreds
    then
      select
      $ from
      $ \defEntry -> do
          -- FIXME (optimization refactor) apply def variant predicates here
          -- let defTag = defEntry  ^. DefEntryDefinitionTag
          -- requires unifying Phrase vs DefEntry distinction. then it just
          -- becomes the above code (convert variants to tags compare tags)
          --
          --let
          --  toDefQueryVariant :: PhraseOrDef -> DefTag -> P.DefQueryVariant
          --  toDefQueryVariant Phrase'     _      = P.Phrase'
          --  toDefQueryVariant Definition' defTag = case defTag of
          --    Headwords'  -> P.Defn'
          --    Inline'     -> P.InlineDef'
          --    Comparison' -> P.DefVersus'

          --  -- WARNING: this will return true when given an empty list
          --  variantConstraint
          --    :: SqlExpr (Value PhraseOrDef) -> SqlExpr (Value DefTag) -> [P.DefQueryVariant] -> SqlExpr (Value Bool)
          let variantConstraint _ [] = val True
              variantConstraint defTag variants =
                 foldl' (\res v -> (val v ==. defTag) ||. res) (val True) variants

          where_
            (   dateConstraint defEntry
            &&. matchesInfixSearches defEntry
            -- FIXME (blocked on tag refactor) JANK WORKAROUND
            &&. variantConstraint (defEntry ^. DefEntryDefinitionTag) querysDefTags

            -- FIXME blocked on subsumption of Phrase by Def -- phrase is more
            -- a tag than a structural distinction
            -- &&. (variantConstraint (defEntry ^. DefEntryPhraseOrDef)
            --                       (defEntry ^. DefEntryDefinitionTag)
            --                       defVariants
            --    ) -- FIXME tag check here!
                 -- * each entry has a json encoded tag in its definitions
                 -- field (one of Inlines, Headwords, (the string "headwords" 
                 -- occurs more than once for def versus comparison entries)
                 -- * and in the "phrase_or_def" field one of Definition' or
                 -- Phrase'
                 --
                 -- we need a filter to take a list of DefQueryVariant 
                 -- ( Phrase' | Defn' | InlineDef' | DefVersus'), 
                 -- the entry's definition tag (Headwords' | Inline' | Comparison), and
                 -- the entry's phrase_or_def (Phrase' | Definiton')
            )
          return defEntry
    else select $ from $ \(defEntry `LeftOuterJoin` readEntry) -> do
      on
        (defEntry ^. DefEntryAttributionTag ==. just (readEntry ^. ReadEntryId))
      where_
        $ attributionConstraint (just $ readEntry ^. ReadEntryAuthor) authPreds
        &&. attributionConstraint (just $ readEntry ^. ReadEntryTitle)
                                  titlePreds
        &&. dateConstraint defEntry
      return defEntry





-- | Definition filter. 
--
-- This function converts the @Entity DefEntry' into a 'DefQuery' before
-- applying any predicates.
--
--
-- Supports for all definition types the following
-- predicates:
--
--  * headword infix search; and 
--  * meaning infix search.
--
--  Additionally, definition subtype filters allow for the selection
--  specifically of:
--
--  * phrases;
--  * inline definitions; and
--  * comparisions.
--
--  By default, passing the @--phrases@ or @--phr@ flag will exclude not only
--  other entry variants but other definition variants as well. However, should
--  the user pass two definition variant flags in a single invocation, both
--  variants will be included in the search results.
--
--  Note that since comparisons consist of two inline definitions, the flag
--  @--inline@ will return both 'InlineDef's and 'DefVersus's. To request /only/
--  inline definitions, and not comparisions, pass the @--only-inline@ flag
--  instead.
--
--  Note as well that this function should only be applied to entries that have
--  satisfied the given date range and author/title predicates, as those tests
--  are sufficiently general to have been factored out--this function relies
--  only on a 'DefQuery' specific search configuration, 'DefSearch' specified below.
--
--
--  TODO This could be vastly simplified by the tag refactor enabled
--  unification of 'Phrase' and 'DefEntry'. Further, the common aspects of all
--  of the search functions might be easily factored out into one generic
--  function.
--
filterDef :: DefSearch -> Entity DefEntry -> Either String (Maybe Result)
filterDef DefSearch { defVariants, headwordPreds, meaningPreds } Entity { entityVal, entityKey }
  = do
    let variantSatisfies
          :: [P.DefQueryVariant] -> Either Phrase P.DefQuery -> Bool
        variantSatisfies variants x = or (defHasType <$> variants <*> pure x)
        (DefEntryKey ts) = entityKey 
    entry <- toEntry entityVal :: Either String Entry
    (\b -> if b then Just $ TsR ts entry else Nothing) <$> case entry of
      Def x@(P.Defn mPg hws) ->
        Right $ variantSatisfies defVariants (Right x) && and
          (headwordPreds <*> hws)
      Def x@(P.InlineDef hw mn) ->
        Right
          $  variantSatisfies defVariants (Right x)
          && and (headwordPreds <*> pure hw)
          && and (meaningPreds <*> pure mn)
      Def x@(P.DefVersus hw mn hw' mn') ->
        Right
          $ -- only one of the two compared definitions need satisfy
             variantSatisfies defVariants (Right x)
          && (  (  and (headwordPreds <*> pure hw)
                && and (meaningPreds <*> pure mn)
                )
             || (  and (headwordPreds <*> pure hw')
                && and (meaningPreds <*> pure mn')
                )
             )
      Phr x@(Plural hws) ->
        Right $ variantSatisfies defVariants (Left x) && and
          (headwordPreds <*> hws)
      Phr x@(Defined hw mn) ->
        Right
          $  variantSatisfies defVariants (Left x)
          && and (headwordPreds <*> pure hw)
          && and (meaningPreds <*> pure mn)
      _ ->
        Left
          "filterDef: expected 'toEntry (entity :: Entity DefEntry)' to be a 'Def' or a 'Phr'\n\
                       \           but found another 'Entry' variant."
filterDefR :: DefSearchR -> Entity DefEntry -> Either String (Maybe Result)
filterDefR DefSearchR { defVariantsR, headwordPredsR, meaningPredsR } Entity { entityVal, entityKey }
  = do
    let variantSatisfies
          :: [P.DefQueryVariant] -> Either Phrase P.DefQuery -> Bool
        variantSatisfies variants x = or (defHasType <$> variants <*> pure x)
        (DefEntryKey ts)   = entityKey
        applyHeadwordPreds = fmap (applyBoolExpr headwordPredsR)
    entry <- toEntry entityVal :: Either String Entry
    (\b -> if b then Just $ TsR ts entry else Nothing) <$> case entry of
      Def x@(P.Defn mPg hws) ->
        Right $ variantSatisfies defVariantsR (Right x) && and
          (applyBoolExpr headwordPredsR <$> hws)
      Def x@(P.InlineDef hw mn) ->
        Right
          $  variantSatisfies defVariantsR (Right x)
          && applyBoolExpr headwordPredsR hw
          && applyBoolExpr meaningPredsR  mn
      Def x@(P.DefVersus hw mn hw' mn') ->
        Right
          $ -- only one of the two compared definitions need satisfy
             variantSatisfies defVariantsR (Right x)
          && (  (  applyBoolExpr headwordPredsR hw
                && applyBoolExpr meaningPredsR  mn
                )
             || (  applyBoolExpr headwordPredsR hw'
                && applyBoolExpr meaningPredsR  mn'
                )
             )
      Phr x@(Plural hws) ->
        Right $ variantSatisfies defVariantsR (Left x) && and
          (applyHeadwordPreds hws)
      Phr x@(Defined hw mn) ->
        Right
          $  variantSatisfies defVariantsR (Left x)
          && applyBoolExpr headwordPredsR hw
          && applyBoolExpr meaningPredsR  mn
      _ ->
        Left
          "filterDef: expected 'toEntry (entity :: Entity DefEntry)' to be a 'Def' or a 'Phr'\n\
                       \           but found another 'Entry' variant."




--- DUMP SEARCH 
--
-- TODO We're leaving this unimplemented as I've never once felt the need to
-- search dumps. The only reason dumps are a present is to prevent parse
-- failures on those early muse logs which have dumps.

-- | Search predicates applied in infix fashion to dump strings--I've never
-- actually used this functionality through the old interface.
type DumpSearch = [String -> Bool]


-- | Apply predicates to a dump.
filterDump :: DumpSearch -> Entity DumpEntry -> Bool
filterDump dumpPreds (Entity _ (DumpEntry dumps)) =
  and ((dumps &) <$> dumpPreds)


filterDumps
  ::  -- MonadIO m => 
     Day -> Day -> [String] -> DB m [String]
filterDumps since before dumpPreds = undefined -- TODO decide on sqlite dump storage


--- DIALOGUE SEARCH

type DialogueSearch = [String]

-- | Apply predicates to a dialogue.
filterDialogue :: DialogueSearch -> Entity DialogueEntry -> Bool
filterDialogue dialoguePreds (Entity _ (DialogueEntry body _)) =
  and (isInfixOf <$> dialoguePreds <*> pure body)


filterDialogues
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> [String]
  -> DB m [Either String Result]
filterDialogues since before authPreds titlePreds dialoguePreds
  = let
      selectWrapper constraints = if null authPreds && null titlePreds
        then select $ from $ \de -> constraints de
        else select $ from $ \(dialogueEntry `LeftOuterJoin` readEntry) -> do
          on
            (   dialogueEntry
            ^.  DialogueEntryAttributionTag
            ==. readEntry
            ?.  ReadEntryId
            )
          constraints dialogueEntry
          return dialogueEntry
     in do ds <- selectWrapper $ \dialogueEntry -> do
                            where_
                              $   dateRangeConstraint DialogueEntryId
                                                      DialogueEntryKey
                                                      dialogueEntry
                                                      since
                                                      before
                              &&. foldr
                                    (\pred rest ->
                                      (dialogueEntry ^. DialogueEntryBody `like` val pred) &&. rest
                                    )
                                    (val True)
                                    dialoguePreds
                            return dialogueEntry

           return $ fmap (\(Entity (DialogueEntryKey ts) entry) -> entryToResult ts entry) ds



--- COMMENTARY SEARCH

type CommentarySearch = [String]

-- | Apply predicates to a commentary.
filterCommentary :: CommentarySearch -> Entity CommentaryEntry -> Bool
filterCommentary commentPreds (Entity _ (CommentaryEntry body _)) =
  and (isInfixOf <$> commentPreds <*> pure body)

filterCommentaries'
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> CommentarySearch
  -> DB m [Either String Result]
filterCommentaries' since before authPreds titlePreds commentaryPreds = do
  cs <- filterCommentaries since before authPreds titlePreds commentaryPreds
  return $ fmap
    (\(Entity (CommentaryEntryKey ts) commentaryEntry) ->
      entryToResult ts commentaryEntry
    )
    cs

filterCommentaries
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> CommentarySearch
  -> DB m [Entity CommentaryEntry]
filterCommentaries since before authPreds titlePreds commentaryPreds =
  let
    selectWrapper constraints = if null authPreds && null titlePreds
      then select $ from $ \commentaryEntry -> constraints commentaryEntry
      else select $ from $ \(commentaryEntry `LeftOuterJoin` readEntry) -> do
        on
          (   commentaryEntry
          ^.  CommentaryEntryAttributionTag
          ==. readEntry
          ?.  ReadEntryId
          )
        constraints commentaryEntry
        return commentaryEntry
  in  selectWrapper $ \commentaryEntry -> do

      where_
        $   dateRangeConstraint CommentaryEntryId
                                CommentaryEntryKey
                                commentaryEntry
                                since
                                before
        &&. foldr
              (\pred rest ->
                (commentaryEntry ^. CommentaryEntryBody `like` val pred)
                  &&. rest
              )
              (val True)
              commentaryPreds
      return commentaryEntry




--- QUOTATION SEARCH

type QuotationSearch = [String]

quoteBodySearchConstraint
  :: SqlExpr (Value String) -> [String] -> SqlExpr (Value Bool)
quoteBodySearchConstraint quoteBody = foldr
  (\searchStr rest -> (quoteBody `like` val searchStr) &&. rest)
  (val True)

-- | Asserts that a text fragment matches all of a list of searches
genericSearchConstraint
  :: SqlString a => SqlExpr (Value a) -> [a] -> SqlExpr (Value Bool)
genericSearchConstraint textToSearch =
  foldr (\query rest -> (textToSearch `like` val query) &&. rest) (val True)

-- | Asserts that a text fragment matches all of a list of searches
genericSearchConstraintOr
  :: SqlString a => SqlExpr (Value a) -> [a] -> SqlExpr (Value Bool)
genericSearchConstraintOr textToSearch =
  foldr (\query rest -> (textToSearch `like` val query) ||. rest) (val False)





-- | 
--  We want to produce sql that is a generalization of the below: 
--
--  @ 
--  select re.id, title, author, manual_attribution, qe.body 
--    from read_entry re, quote_entry qe 
--    where qe.attribution_tag == re.id 
--      and (author like "%Woolf%" or manual_attribution like "%Woolf%") 
--      and qe.body like "%simplicity%";
--
-- @
-- or 
--
-- @ 
-- select title, author, manual_attribution, body 
--    from quote_entry  left join read_entry on read_entry.id=quote_entry.attribution_tag; 
-- @
--
filterQuotes
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> [String]
  -> DB m [Entity QuoteEntry]
filterQuotes since before authPreds titlePreds bodyStrs =
  select
    $ from
    $ \(quoteEntry `LeftOuterJoin` readEntry) -> do
        on
          (quoteEntry ^. QuoteEntryAttributionTag ==. (readEntry ?. ReadEntryId)
          )
        where_
          (
           -- satisfies title/auth preds if present
           (
              -- manual attr satsifies
            (attributionConstraint (quoteEntry ^. QuoteEntryManualAttribution)
                                   authPreds
            &&. attributionConstraint
                  (quoteEntry ^. QuoteEntryManualAttribution)
                  titlePreds
            )
           ||.
            -- tagged attr satsfies
               (attributionConstraint (readEntry ?. ReadEntryAuthor) authPreds
               &&. attributionConstraint
                     (readEntry ?. ReadEntryTitle)
                     titlePreds
               )
           )
          &&. -- satisfies quoteBody preds
              quoteBodySearchConstraint (quoteEntry ^. QuoteEntryBody)
                                        bodyStrs
          &&. dateRangeConstraint QuoteEntryId
                                  QuoteEntryKey
                                  quoteEntry
                                  since
                                  before
          )
        return quoteEntry

filterQuotes''
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> [String]
  -> DB m [Entity QuoteEntry]
filterQuotes'' since before authPreds titlePreds bodyStrs =
  select
    $ from
    $ \(quoteEntry `LeftOuterJoin` readEntry) -> do
        on
          (quoteEntry ^. QuoteEntryAttributionTag ==. (readEntry ?. ReadEntryId)
          )
        where_
          (
           -- satisfies title/auth preds if present
           (
              -- manual attr satsifies
            (attributionConstraint (quoteEntry ^. QuoteEntryManualAttribution)
                                   authPreds
            &&. attributionConstraint
                  (quoteEntry ^. QuoteEntryManualAttribution)
                  titlePreds
            )
           ||.
            -- tagged attr satsfies
               (attributionConstraint (readEntry ?. ReadEntryAuthor) authPreds
               &&. attributionConstraint
                     (readEntry ?. ReadEntryTitle)
                     titlePreds
               )
           )
          &&. -- satisfies quoteBody preds
              quoteBodySearchConstraint (quoteEntry ^. QuoteEntryBody)
                                        bodyStrs
          -- &&. dateRangeConstraint QuoteEntryId
          --                         QuoteEntryKey
          --                         quoteEntry
          --                         since
          --                         before
          )
        return quoteEntry

filterQuotes'
  :: MonadIO m
  => Day
  -> Day
  -> [String]
  -> [String]
  -> [String]
  -> DB m [Either String Result]
filterQuotes' since before auth title quoteBody = do
  qs <- filterQuotes since before auth title quoteBody
  return $ fmap (\(Entity (QuoteEntryKey ts) quoteEntry) -> entryToResult ts quoteEntry) qs

clearDb :: T.Text -> IO ()
clearDb db = runSqlite db $ do
  runMigration migrateAll
  deleteWhere ([] :: [P.Filter DefEntry])
  deleteWhere ([] :: [P.Filter ReadEntry])
  deleteWhere ([] :: [P.Filter QuoteEntry])
  deleteWhere ([] :: [P.Filter CommentaryEntry])
  deleteWhere ([] :: [P.Filter DialogueEntry])
  deleteWhere ([] :: [P.Filter PageNumberEntry])
