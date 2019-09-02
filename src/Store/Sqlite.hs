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

module Store.Sqlite where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )
import           Data.Aeson hiding (Result)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC
import           Data.Either
import           Data.Function                  ( (&) )
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( catMaybes )
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
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Debug.Trace                    ( trace )
import           GHC.Generics
import qualified Parse                         as P
import           Parse.Entry             hiding ( DefQueryVariant(..) )
import qualified Parse.Entry                   as P
import qualified Parse.Entry             hiding ( DefQueryVariant(..) )
import           Search
import           Store                          ( Result(..) )
import           Store.Sqlite.Types
import           Store.Types                    ( AttrTag(..) )
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )
import           Time
import           Time                           ( toUTC )
import           Web.PathPieces                 ( PathPiece(..) )


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


data EntryType = ERead | EQuote | EDef | ECommentary | EDialogue | EPageNumber | EDump deriving (Eq, Show)

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

data DefEntry'
  = Headwords [TL.Text]
  | Inlines [InlineDef]
  deriving (Eq, Show, Generic)

instance ToJSON DefEntry' where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefEntry'

-- | JSON-encodable 'DumpEntry'. A 'DumpEntry'' is a list of 'Dump's belonging
-- to a single 'Day' (its primary key).
data DumpEntry' = DumpEntry'
  { dumps :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DumpEntry' where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DumpEntry'

type DB m a = ReaderT SqlBackend m a

fromPageNum :: P.PageNum -> (PageTag, Int)
fromPageNum pageNum =
  case pageNum of
    (P.Page pg) -> (Page', fromIntegral pg)
    (P.PStart pg) -> (PStart', fromIntegral pg)
    (P.PEnd pg) -> (PEnd', fromIntegral pg)
    (P.PFinish pg) -> (PFinish', fromIntegral pg)

-- | Converts an 'AttrTag' into a primary key of the 'ReadEntry' table; that
-- is, a wrapped 'UTCTime'.
fromAttrTag :: AttrTag -> Key ReadEntry
fromAttrTag = ReadEntryKey . attrId

-- | Writes and tags a 'Day'\'s 'LogEntry's to the database. A singly indented
-- entry is tagged with the id of the top-level parent the least lines above
-- it; if no such parent exists the entry is left untagged and a (TODO) warning is
-- logged.
--
-- TODO works in 'demo': convert back to  @[LogEntry]@ to test that
-- @(writeEntry .  readRead) == id@ is true for any entry and that attributions
-- are correctly determined (which requires manual tagging).
writeDay :: MonadIO m => Day -> [LogEntry] -> DB m [Either String UTCTime]
writeDay day =
  let go ::
           MonadIO m
        => Maybe AttrTag
        -> [LogEntry]
        -> DB m [Either String UTCTime]
      go _ [] = return []
      go mTag (h:t)
        -- a. toplevel and read entry -> insert, create tag from id and place in (optional) accumulator, recurse
        | isTopLevel h && isRead h = do
          eitherId <- writeLogEntry day h Nothing
          case eitherId
                 -- WARNING: this is impossible: it means that the entry is a Read
                 -- variant and a Dump variant, so if it occurs, it will have
                 -- been by programmer error. As such, we simply log the
                 -- irregularity, 
                of
            Left err ->
              (:) <$>
              (return $
               Left $
               "WARNING: writeDay: found entry that is both Read and Dump\ncont.: " ++
               err) <*>
              go Nothing t
            -- begin traversal with readEntryKey in accumulator (note that it
            -- should be immediately discarded if the next entry is not the
            -- child of the current read entry).
            Right readEntryKey ->
              go
                (Just $
                 AttrTag
                   (trace ("readEntryKey: " ++ show readEntryKey) readEntryKey))
                t
        -- b. toplevel and not read entry ->  insert without tag, recurse with accumulator := Nothing 
        | isTopLevel h
          -- discards accumulator should the last entry have been of the Read
          -- variant
         = (:) <$> writeLogEntry day h Nothing <*> go Nothing t
        -- d. indented one level or more, read entry -> 
        --      insert untagged, log warning describing weird nested readings (now 
        --      this may warrant future support, but since we're only replicating 
        --      the existing behavior of muse, this is out of scope.), and pass
        --      previous accumulator through.
        | isIndentedTo 1 h && isRead h =
          (\h h' t -> h : h' : t) <$>
          (return $
           Left "WARNING: writeDay: found indented read entry, leaving untagged") <*>
          writeLogEntry day h Nothing -- NOTE: leaving read entries untagged
           <*>
          go mTag t
        -- c. indented one level or more, not read entry ->
        --      i.  accumulator is Nothing -> insert indented untagged, recurse
        --      with previous accumulator
        --      ii. accumulator is (Just attrTag) -> insert tagged, recurse
        --      with previous accumulator
        | isIndentedTo 1 h
            --(:) <$> writeLogEntry day h (trace (show mTag) mTag) <*> go mTag t
         = (:) <$> writeLogEntry day h mTag <*> go mTag t
        -- otherwise insert untagged, recurse with accumulator := Nothing
        | otherwise = (:) <$> writeLogEntry day h Nothing <*> go Nothing t
  in go Nothing

-- | A write action for a single 'LogEntry'. It's intended for application to
-- lists of log entries grouped by day: for each day's list of log entries,
-- 'writeEntry' is partially applied to the 'Day' and mapped over the entries.
--
writeLogEntry ::
     MonadIO m
  => Day
  -> LogEntry
  -> Maybe AttrTag
  -> DB m (Either String UTCTime)
writeLogEntry day logEntry mAttrTag =
  trace ("writeLgEntry: " ++ show mAttrTag) $
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
    TabTsEntry (indentation, timestamp, entry) ->
      let utc = toUTC day timestamp
      in (case entry of
            Def dq -> insertKey (DefEntryKey utc) $ toDefEntry mAttrTag dq
            Read title author ->
              insertKey (ReadEntryKey utc) $ ReadEntry title author
                          -- FIXME relies on empty attribution string when manual
                          -- attribution is omitted
            Quotation quoteBody manualAttribution mPgNum ->
              insertKey (QuoteEntryKey utc) $
              QuoteEntry
                quoteBody
                (if manualAttribution == ""
                   then Nothing
                   else Just manualAttribution)
                (fromAttrTag <$> mAttrTag)
            Commentary commentBody ->
              insertKey (CommentaryEntryKey utc) $
              CommentaryEntry commentBody $ fromAttrTag <$> mAttrTag
            PN pageNum ->
              let (pageTag, pgNum) = fromPageNum pageNum
              in insertKey (PageNumberEntryKey utc) $
                 PageNumberEntry pgNum pageTag (fromAttrTag <$> mAttrTag)
            Phr phrase ->
              insertKey (DefEntryKey utc) $ phraseToDefEntry mAttrTag phrase
            Dialogue dialogueBody ->
              insertKey (DialogueEntryKey utc) $
              DialogueEntry dialogueBody $ fromAttrTag <$> mAttrTag
            Parse.Entry.Null -> return ()) >>
         (return $ Right utc)

main :: IO ()
main
  --runSqlite ":memory:" $ do
 =
  runSqlite "test.db" $ do
    runMigration migrateAll
    -- read entry
    now <- liftIO $ getCurrentTime
    let wutheringHeightsId = (ReadEntryKey now)
    insertKey wutheringHeightsId $ ReadEntry "Wuthering Heights" "Emily Bronté"
    -- def entry
    now' <- liftIO $ getCurrentTime
    let inline =
          TL.decodeUtf8 $
          encode $ Inlines [InlineDef "mizzle" "a misty drizzle"]
        defEntryId = DefEntryKey now'
    insertKey defEntryId $
      DefEntry
        inline
        Inline'
        (Just wutheringHeightsId)
        Definition'
        Nothing
        Nothing
    (allDefs :: [Entity DefEntry]) <- selectList [] []
    (allReads :: [Entity ReadEntry]) <- selectList [] []
    liftIO $
      BLC.putStrLn $ encode $ Inlines [InlineDef "mizzle" "a misty drizzle"]
    let entry :: Either String Entry
        entry =
          toEntry $
          DefEntry
            (TL.decodeUtf8 $
             encode $ Inlines [InlineDef "mizzle" "a misty drizzle"])
            Inline'
            Nothing
            Definition'
            Nothing
            Nothing
    liftIO $ putStr "entry: " >> pPrint entry
    liftIO $
      pPrint $
      let xs :: [Either String Entry]
          xs = fmap (toEntry . entityVal) allDefs
      in rights xs
    liftIO $ pPrint $ fmap entityVal allReads
    -- clean up 
    deleteWhere [DefEntryId ==. defEntryId]
    deleteWhere [ReadEntryId ==. wutheringHeightsId]
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

class Show err =>
      ToEntry err a where
  toEntry :: a -> Either err Entry

instance ToEntry String DefEntry where
  toEntry (DefEntry {..}) -- Right $ Quotation "" "" Nothing
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
                  ((InlineDef {..}):[]) ->
                    Right $
                    Def $ P.InlineDef (T.unpack headword) (T.unpack meaning)
                  (h:t:[]) ->
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
                  ((InlineDef {..}):[]) ->
                    Right $ Phr $ Defined (T.unpack headword) (T.unpack meaning)
                  _ ->
                    Left
                      "toEntry: phrase: found more than two inline defs, corrupt input."
      Left err -> Left err

instance ToEntry String ReadEntry where
  toEntry (ReadEntry {..}) = pure $ Read readEntryTitle readEntryAuthor

instance ToEntry String QuoteEntry where
  toEntry (QuoteEntry {..}) =
    pure $
    Quotation quoteEntryBody (maybe "" id quoteEntryManualAttribution) Nothing -- FIXME PageNum support

instance ToEntry String CommentaryEntry where
  toEntry = pure . Commentary . commentaryEntryBody -- FIXME attribution discarded

instance ToEntry String DialogueEntry where
  toEntry = pure . Dialogue . dialogueEntryBody -- FIXME attribution discarded

instance ToEntry String PageNumberEntry where
  toEntry (PageNumberEntry {..}) =
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
toDefEntry mAttrTag dq =
  case dq of
    P.Defn mPg hws ->
      DefEntry
        (TL.decodeUtf8 $ encode $ Headwords $ TL.pack <$> hws)
        Headwords'
        (fromAttrTag <$> mAttrTag)
        Definition'
        (fromIntegral <$> mPg)
        Nothing
    P.InlineDef headword meaning ->
      DefEntry
        (TL.decodeUtf8 $
         encode $ Inlines [InlineDef (T.pack headword) (T.pack meaning)])
        Inline'
        (fromAttrTag <$> mAttrTag)
        Definition'
        -- TODO add page nums (prefix types?) for all definition/phrase types.
        Nothing
        Nothing
    P.DefVersus headword meaning headword' meaning' ->
      DefEntry
        (TL.decodeUtf8 $
         encode $
         Inlines
           [ InlineDef (T.pack headword) (T.pack meaning)
           , InlineDef (T.pack headword') (T.pack meaning')
           ])
        Comparison'
        (fromAttrTag <$> mAttrTag)
        Definition'
        -- TODO add page nums (prefix types?) for all definition/phrase types.
        Nothing
        Nothing

-- | Like 'toDefEntry' but converts from 
phraseToDefEntry :: Maybe AttrTag -> Phrase -> DefEntry
phraseToDefEntry mAttrTag phrase =
  case phrase of
    Plural hws ->
      DefEntry
        (TL.decodeUtf8 $ encode $ Headwords $ TL.pack <$> hws)
        Headwords'
        (fromAttrTag <$> mAttrTag)
        Phrase'
        Nothing
        Nothing
    Defined headword meaning ->
      DefEntry
        (TL.decodeUtf8 $
         encode $ Inlines [InlineDef (T.pack headword) (T.pack meaning)])
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
  , checkPhrases :: Bool
  , checkQuotes :: Bool
  , checkDialogues :: Bool
  , checkComments :: Bool
  , checkDefinitions :: Bool
  , attributionPreds :: [Title -> Author -> Bool]
  , definitionSearch :: DefSearch
  , quoteSearch :: QuotationSearch
  , commentarySearch :: CommentarySearch
  , dialogueSearch :: DialogueSearch
  , dumpSearch :: DumpSearch
  }


-- | Dispatch search.
dispatchSearch :: MonadIO m => SearchConfig -> DB m [Result]
dispatchSearch SearchConfig {..} =
  let withinRange = undefined
      -- x :: Int
      x = 
        [ -- if checkDumps
        ]
   in  undefined


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
            maybe True id
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
  => [String] -- | Author predicates
  -> [String]
  -> [Entity record]
  -> DB m [Entity record]
applyReadPreds authorPreds titlePreds entities =
  fmap catMaybes
    $ sequence
    $ let x = taggedEntrySatisfies authorPreds titlePreds <$> entities
      in  trace ("applyReadPreds: \n" <> "# results: " <> show (length x)) x

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
      [entryIdType >=. (keyWrapper since), entryIdType <=. (keyWrapper before)]
  in
    selectList dateConstraints []


--- DEFINITION SEARCH 

-- | Search predicates specifically for definitions. See 'filterDef'\'s
-- documentation for more details.
data DefSearch = DefSearch
  { defVariants :: [P.DefQueryVariant]
  , headwordPreds :: [String -> Bool]
  , meaningPreds :: [String -> Bool] }


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
filterDef :: DefSearch -> Entity DefEntry -> Either String Bool
filterDef DefSearch { defVariants, headwordPreds, meaningPreds } Entity { entityVal }
  = do
    let variantSatisfies
          :: [P.DefQueryVariant] -> Either Phrase P.DefQuery -> Bool
        variantSatisfies variants x = or (defHasType <$> variants <*> pure x)
    entry <- (toEntry entityVal :: Either String Entry)
    case entry of
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


--- DUMP SEARCH

-- | Search predicates applied in infix fashion to dump strings--I've never
-- actually used this functionality through the old interface.
type DumpSearch = [String -> Bool]


-- | Apply predicates to a dump.
filterDump :: DumpSearch -> Entity DumpEntry -> Bool
filterDump dumpPreds (Entity _ (DumpEntry dumps)) =
  and ((dumps &) <$> dumpPreds)


--- DIALOGUE SEARCH

type DialogueSearch = [String -> Bool]

-- | Apply predicates to a dialogue.
filterDialogue :: DialogueSearch -> Entity DialogueEntry -> Bool
filterDialogue dialoguePreds (Entity _ (DialogueEntry body _)) =
  and ((body &) <$> dialoguePreds)


--- COMMENTARY SEARCH

type CommentarySearch = [String -> Bool]

-- | Apply predicates to a commentary.
filterCommentary :: CommentarySearch -> Entity CommentaryEntry -> Bool
filterCommentary commentPreds (Entity _ (CommentaryEntry body _)) =
  and ((body &) <$> commentPreds)


--- QUOTATION SEARCH

type QuotationSearch = [String -> Bool]

filterQuote :: QuotationSearch -> Entity QuoteEntry -> Bool
filterQuote quotePreds (Entity _ (QuoteEntry body _ _)) =
  and ((body &) <$> quotePreds)
