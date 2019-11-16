{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, MultiWayIf, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Search
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module exposes search predicate application functions.
-----------------------------------------------------------------------------
module Search where

import           Control.Lens                   ( _Left
                                                , _Right
                                                , makeLenses
                                                , preview
                                                , review
                                                )
import           Control.Lens.TH                ( makeLenses
                                                , makePrisms
                                                )
import           Control.Lens.Tuple
import           Control.Monad                  ( (>=>)
                                                , join
                                                , void
                                                )
import           Data.Aeson              hiding ( Null )
import           Data.Bifunctor
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                , sort
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                , isNothing
                                                , mapMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text.Metrics              ( levenshtein )
import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Clock                ( utctDay )
import           Data.Yaml.Config               ( load
                                                , lookup
                                                , lookupDefault
                                                , subconfig
                                                )
import           Debug.Trace                    ( trace )
import           Helpers
import           Parse
import           Parse.Entry
import           Prelude                 hiding ( log
                                                , lookup
                                                , min
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , listDirectory
                                                )
import           System.Environment             ( getEnv )
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import qualified Text.Trifecta                 as Tri
import qualified Text.Trifecta.Result          as Tri

-- | Represents filters and entry maps extracted from CLI invocation.
data Input = Input
  { startDateTime :: Day
  , endDateTime :: Day
  , authorPred :: Maybe (LogEntry -> Bool)
  , titlePred :: Maybe (LogEntry -> Bool)
  , predicates :: [Maybe (LogEntry -> Bool)]
  }

-- TODO replace w/ newtype wrapper around `Entry`
data SearchResult
  = Def' String
  | Quotation' Quote
               Author
               (Maybe PgNum)
  | Commentary' Body
  | Read' Title
          Author
  | PN' PageNum
  | Null'
  | Entry' String -- ?
  deriving (Eq, Show)

x :: Input -> [LogEntry]
x = undefined

-- | How to filter by date:
--
-- 1. extract month span from date range
-- 2. fetch dates within range
--
-- 1. covert filepaths to Days.
--    - print out invalid dates/names, parse errors
--    -
pathsToDays' :: [FilePath] -> [(String, Either String (Maybe Day))]
pathsToDays' = fmap $ \fp -> (fp, showErr $ parse day' fp)

pathToDay :: FilePath -> Maybe Day
pathToDay = join . preview _Right . showErr . parse day'

showInvalidNames :: [(String, Either String (Maybe Day))] -> IO [Day]
showInvalidNames = foldr f (return [])
 where
  f (fp, e) r = case e of
    Left  err  -> putStrLn ("Failed to parse date:\n" ++ err) >> r
    Right mday -> case mday of
      Just d  -> (d :) <$> r
      Nothing -> putStrLn ("File named invalid date: " ++ fp) >> r

pathsToDays :: [FilePath] -> IO [Day]
pathsToDays = showInvalidNames . pathsToDays'

dayToPath :: Day -> String
dayToPath = replace '-' '.' . drop 2 . show

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if a == x then b else x)

filterBy
  :: Day -- ^ start date (lower bound)
  -> Day -- ^ end date (upper bound)
  -> [Day]
  -> [Day]
filterBy l u = filter (\d -> d >= l && d <= u)

-- auto-complete
-- Qs:
--
-- * add "canonical <auth>" entry to log seeds for entity recognition via
--   `rate`?
-- * add entity recog. checking linter option to muse CLI; could be run on save
--   thereby showing errors in (nearly) real-time which allows for on the spot
--   user correction.
--
--   This linter would:
--
--      1. grab line at cursor
--      2. try to extract entry w citation/attribution, a.t.m. only `Read`
--      3. rate against last ~5 authors (take first match); otherwise lazily
--         crawl author history in chunks of size, say, 10
--      4. display (i) the identified author or (ii) that it is unrecognized,
--         with a list of the top, say, 5 matches above the distance threshold.
--
--      tl;dr; try to identify attr under cursor, if successful display result,
--      if not show top matches (above satisfaction threshold)
idea = undefined

-- | Where there is neither a title predicate nor an author predicate, applies
-- only variant (a.t.m. def and quote) filters; otherwise, where there is at
-- least one `Read` predicate, filters by nesting and applies variant filters.
filterWith' :: Input -> [LogEntry] -> [LogEntry]
filterWith' _ [] = []
filterWith' input (x : xs)
  |
  -- no string search predicates; does no recurse
    isNothing (titlePred input) && isNothing (authorPred input) =
    -- old: filter (\e -> isRead e || variantSatisfies input e) (x:xs)
                                                                  filter'
    (x : xs)
  |
  -- string search preds present; type preds poss.
  -- that is, collect entries which are nested w `variantSatisfies`; outside of
  -- nested collect nothing but quotations or read entries that satisfy
  -- (`searchSatisfy`)
    otherwise = go (x : xs)
 where
  filter' :: [LogEntry] -> [LogEntry]
  filter' [] = []
  filter' (x : xs) =
    case compareRead <$> projectRead x <*> (head' rest >>= projectRead) of
      Just True -> rest
      _         -> if
        | isRead x || variantSatisfies input x -> x : rest
        | otherwise                            -> rest
    where rest = filter' xs
  go :: [LogEntry] -> [LogEntry]
  go []              = []
  go (Dump _ : xs) = go xs
  go (x@(TabTsEntry (tabs, _, _)) : xs)
    |
    -- collect nested
    -- TODO if read groups are empty, add none of the entries, including the
    -- read entry itself
      doesReadSatisfy input x
    = let (belong, rest) = takeWhileRestWithFilter
            (doesEntryBelongToParent tabs)
            (variantSatisfies input)
            xs
              -- inside filter; include read entries
              -- do not recurse to `filterWith`; lack of search preds holds.
      in  if null belong then go rest else (x : belong) ++ go rest
    | searchSatisfies' input x
    = x : go xs
    | otherwise
    = go xs

-- | Comparse auth, title strings of read entry
compareRead :: (Title, Author) -> (Title, Author) -> Bool
compareRead (t, a) (t', a') = t == t' && a' == a

-- TODO
takeWhileRestWithFilter
  :: (a -> Bool) -- take until this fails
  -> (a -> Bool) -- skip those that fail this
  -> [a]
  -> ([a], [a])
takeWhileRestWithFilter _ _ [] = ([], [])
takeWhileRestWithFilter continue take (x : xs)
  | continue x = go x $ takeWhileRestWithFilter continue take xs
  | otherwise  = ([], x : xs)
 where
  go x (as, bs) | take x    = (x : as, bs)
                | otherwise = (as, bs)

-- | Applies both string search and variant predicates.
satisfies :: Input -> LogEntry -> Bool
satisfies input@(Input _ _ ap tp preds') =
  let preds = catMaybes (predicates input ++ [ap, tp])
  in  if null preds
        then const True -- TODO search bug ??
        else \e -> or $ preds <*> [e]

-- | Applies both string search and variant predicates.
satisfies' :: Input -> LogEntry -> Bool
satisfies' input le
  | isQuote le || isRead le
  = searchSatisfies input le && variantSatisfies input le
  | otherwise
  = variantSatisfies input le

-- | Applies variant predicates (to collect nested entries)
variantSatisfies :: Input -> LogEntry -> Bool
variantSatisfies input | null preds = const True
                       | -- TODO search bug ??
                         otherwise  = \e -> or $ preds <*> [e]
  where preds = catMaybes (predicates input)

-- | Applies only string search predicates.
searchSatisfies :: Input -> LogEntry -> Bool
searchSatisfies (Input _ _ ap tp _) le
  | null res  = True
  | otherwise = and $ catMaybes res
  where res = [ap <*> pure le, tp <*> pure le]

-- | Applies only string search predicates to author, title.
searchSatisfies' :: Input -> LogEntry -> Bool
searchSatisfies' (Input _ _ ap tp _) le
  | null res  = False
  | otherwise = and $ catMaybes res
  where res = [ap <*> pure le, tp <*> pure le]

-- | Checks whether a 'LogEntry' is indented at least as much as the given
-- depth; for 'Dump's returns @False@ when depth >= 1.
isIndentedTo :: Int -> LogEntry -> Bool
isIndentedTo depth (Dump       _                  ) = depth == 0
isIndentedTo depth (TabTsEntry (indentation, _, _)) = indentation >= depth

-- | Checks whether a 'LogEntry' is indented _exactly_ to the given depth;
-- for 'Dump's returns @False@ unless the given depth is 0.
isIndentedExactlyTo :: Int -> LogEntry -> Bool
isIndentedExactlyTo depth (Dump _) = depth == 0
isIndentedExactlyTo depth (TabTsEntry (indentation, _, _)) =
  indentation == depth

-- | Checks whether a 'LogEntry' is not indented.
-- 'Dumps' are considered to be top-level.
isTopLevel :: LogEntry -> Bool
isTopLevel (TabTsEntry (0, _, _)) = True
isTopLevel (Dump       _        ) = True
isTopLevel _                      = False


isDef :: LogEntry -> Bool
isDef = isJust . projectDefQuery

isRead :: LogEntry -> Bool
isRead = isJust . projectRead

isQuote :: LogEntry -> Bool
isQuote = isJust . projectQuotation

isPhrase :: LogEntry -> Bool
isPhrase = isJust . projectPhrase

isDialogue :: LogEntry -> Bool
isDialogue = isJust . projectDialogue

doesAuthorSatisfy :: (Author -> Bool) -> LogEntry -> Bool
doesAuthorSatisfy authPred (TabTsEntry (_, _, Read _ author)) =
  authPred author
doesAuthorSatisfy _ _ = False

doesTitleSatisfy :: (Title -> Bool) -> LogEntry -> Bool
doesTitleSatisfy titlePred (TabTsEntry (_, _, Read title _)) =
  titlePred title
doesTitleSatisfy _ _ = False

-- | For each present read predicate (a.t.m. only on author and title strings)
-- apply both to `LogEntry` if it's `Read` of `Entry`.
doesReadSatisfy :: Input -> LogEntry -> Bool
doesReadSatisfy input le
  | isRead le
  = or $ mapMaybe (Just input >>=) [authorPred, titlePred] <*> pure le
  | otherwise
  = False

-- | Takes parent (read entry's) indentation level, a log entry
doesEntryBelongToParent :: Int -> LogEntry -> Bool
doesEntryBelongToParent tabs (Dump _) = False -- breaks on dumps
doesEntryBelongToParent tabs (TabTsEntry (tabs', _, _)) | tabs' > tabs = True
                                                        | otherwise    = False

-- | Consumes title or author serach string and type predicates (e.g., `isDef`,
-- `isQuote`, etc.) and creates a composite predicate that will only apply the
-- former where the latter have succeeded. Includes "Read" entries by default,
-- for labelling.
guardStrSearch
  :: (LogEntry -> Bool) -- converted auth/title string search
  -> [Maybe (LogEntry -> Bool)] -- `LogEntry` variant preds
  -> (LogEntry -> Bool)
guardStrSearch strSearch typePreds
  | null typePreds = strSearch
  | otherwise = \e ->
    strSearch e
      && or ((isRead : catMaybes typePreds) <*> pure e)

-- | Generates `LogEntry` predicate from string predicate.
--
-- This only applies string searches to `Quotation`s and `Read`s.
convertAuthSearch :: (Author -> Bool) -> LogEntry -> Bool
convertAuthSearch authPred le
  | isJust read  = let (_, a) = fromJust read in authPred a
  | isJust quote = let (_, attr, _) = fromJust quote in authPred attr
  | otherwise    = False
 where
  read  = projectRead le
  quote = projectQuotation le

-- how safe is this?
-- | Generates `LogEntry` predicate from string predicate.
--
-- This only applies string searches to `Quotation`s and `Read`s.
convertTitleSearch :: (Title -> Bool) -> LogEntry -> Bool
convertTitleSearch titlePred le
  | isJust read  = let (t, _) = fromJust read in titlePred t
  | isJust quote = let (_, attr, _) = fromJust quote in titlePred attr
  | otherwise    = False
 where
  read  = projectRead le
  quote = projectQuotation le

-- how safe is this?
defInp = input Nothing Nothing []

-- | `LogEntry` projections.
getEntry :: LogEntry -> Maybe Entry
getEntry = preview $ _TabTsEntry . _3

getDump :: LogEntry -> Maybe String
getDump = preview _Dump

-- `Entry` projections
getDefQuery :: Entry -> Maybe DefQuery
getDefQuery = preview _Def

getPageNum :: Entry -> Maybe PageNum
getPageNum = preview _PN

getRead :: Entry -> Maybe (Title, Author)
getRead = preview _Read

getQuotation :: Entry -> Maybe (Quote, Attr, Maybe PgNum)
getQuotation = preview _Quotation

getCommentary :: Entry -> Maybe Body
getCommentary = preview _Commentary

getPhrase :: Entry -> Maybe Phrase
getPhrase = preview _Phr

getDialogue :: Entry -> Maybe String
getDialogue = preview _Dialogue

-- Composed `LogEntry` and `Entry` projections.
projectDefQuery :: LogEntry -> Maybe DefQuery
projectDefQuery = getEntry >=> getDefQuery

projectQuotation = getEntry >=> getQuotation

projectPageNum = getEntry >=> getPageNum

projectRead = getEntry >=> getRead

projectCommentary = getEntry >=> getCommentary

projectPhrase = getEntry >=> getPhrase

projectDialogue = getEntry >=> getDialogue

project :: LogEntry -> Maybe DefQuery
project (TabTsEntry (_, _, Def dq)) = Just dq
project _                             = Nothing

projectDef :: LogEntry -> [(Headword, Maybe Meaning)]
projectDef (TabTsEntry (_, _, Def dq)) = case dq of
  Defn      mPg headwords -> (, Nothing) <$> headwords
  InlineDef hw  meaning   -> [(hw, Just meaning)]
  DefVersus hw m hw' m'   -> [(hw, Just m), (hw', Just m')]
projectDef _ = []

input ap tp preds = do
  d <- utctDay <$> getCurrentTime
  return
    $ Input d d (convertAuth preds <$> ap) (convertTitle preds <$> tp) preds

convertAuth preds = flip guardStrSearch preds . convertAuthSearch

convertTitle preds = flip guardStrSearch preds . convertTitleSearch

testLogWithDumpOutput :: [LogEntry]
testLogWithDumpOutput
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf"
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 55, sec = 17}
      , Def
        (DefVersus "benignant"
                   "kind; gracious; favorable;"
                   "benign"
                   "gentle, mild, or, medically, non-threatening"
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 45}
      , Def
        (DefVersus
          "malignant"
          "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
          "malign"
          "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Def (Defn (Just 38) ["inimical", "traduce", "virulent"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
        "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
        "In \"To the Lighthouse\", by Virginia Woolf"
        Nothing
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf"
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation "Her simplicity fathomed what clever people falsified."
                  "In \"To the Lighthouse\", by Virginia Woolf"
                  Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
        "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n"
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf"
        (Just 38)
      )
    ]
