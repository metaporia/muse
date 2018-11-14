{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
   OverloadedStrings, TupleSections, MultiWayIf #-}
module Search where

import           Control.Monad (void, (>=>))
import           Control.Lens.TH (makeLenses, makePrisms)
import           Control.Lens.Tuple
import           Control.Lens (makeLenses, preview, review)
import           Data.Aeson hiding (Null)
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.List (isInfixOf, sort, intercalate)
import           Data.Maybe (catMaybes, isJust, isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Metrics (levenshtein)
import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Clock (utctDay)
import           Debug.Trace (trace)
import           Data.Yaml.Config (load, lookup, lookupDefault, subconfig)
import           Helpers
import           Parse
import           Parse.Entry
import           Prelude hiding (lookup, log, min)
import           System.Directory (doesFileExist, createDirectoryIfMissing, listDirectory)
import           System.Environment (getEnv)
import           Text.Show.Pretty (pPrint)
import qualified Text.Trifecta.Result as Tri
import qualified Text.Trifecta as Tri

-- | Represents filters and entry maps extracted from CLI invocation.
data Input = Input
  { startDateTime :: Day
  , endDateTime :: Day
  , authorPred :: Maybe (Author -> Bool)
  , titlePred :: Maybe (Title -> Bool)
  , definitions :: Bool
  , quotations :: Bool
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
  | Null' | Entry' String -- ?
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
pathsToDays' = fmap $ \fp -> (fp, showErr $ parse day' (fp))

showInvalidNames :: [(String, Either String (Maybe Day))] -> IO [Day]
showInvalidNames es = foldr f (return []) es
  where
    f (fp, e) r =
      case e of
        Left err -> putStrLn ("Failed to parse date:\n" ++ err) >> r
        Right mday ->
          case mday of
            Just d -> (d :) <$> r
            Nothing -> putStrLn ("File named invalid date: " ++ fp) >> r

pathsToDays :: [FilePath] -> IO [Day]
pathsToDays = showInvalidNames . pathsToDays'

dayToPath :: Day -> String
dayToPath = replace '-' '.' . drop 2 . show 

replace :: Eq b => b -> b -> [b] -> [b]
replace a b = map (\x -> if (a == x) then b else x)


filterBy :: Day -- ^ start date (lower bound)
         -> Day -- ^ end date (upper bound)
         -> [Day] 
         -> [Day]
filterBy l u = filter (\d -> d >= l && d <= u) 

type Tag = String
type TagPred = String -> Bool -- Tag predicate.

-- | Generate a tag for an entry; succeeds for `Read` variant.
--   * extract one tag from title/author attr despite variance
genTag :: Entry -> Maybe Tag
genTag (Read title author) = undefined
  where
    initials = (++ ".") . intercalate "." . fmap (take 1 . trim) . words . trim
genTag _ = Nothing

-- | Trims whitespace, slides shorter string over longer, gets each pair's
-- levenshtein distance, returns shortest.
-- TODO make case-insensitive
textDiff :: T.Text -> T.Text -> Int
textDiff a b =
  let trim = T.unwords . T.words
      a' = trim a
      b' = trim b
      la' = T.length a'
      lb' = T.length b'
  in if | la' == lb' -> levenshtein a' b'
        | la' > lb' -> go lb' b' la' a'
        | otherwise -> go la' a' lb' b'
    -- n.b. breaks if short is longer than long. do not tamper w the above
    -- guard
  where
    go :: Int -> T.Text -> Int -> T.Text -> Int
    go ls short ll long =
      sum $ (\w -> go' (T.length w) w ll long) <$> T.words short
      where
        go' ls short ll long =
          minimum . fmap (uncurry levenshtein) $
          zip (repeat short) $ [T.take ls $ T.drop i long | i <- [0 .. ll - ls]]


cowards :: [T.Text]
cowards = ["Noel Coward", "Noël Coward"]

coward :: [T.Text]
coward = ["Noël Coward"]

eliots :: [T.Text]
eliots =
  [ "George Eliot"
  , "George Elliot"
  , "George Elliot (Mary Ann Evans)"
  , "George Eliot (Mary Ann Evans)"
  ]

eliot :: [T.Text]
eliot = ["George Eliot", "Mary Ann Evans"]

dostoevskys :: [T.Text]
dostoevskys =
  [ "Fyodor Dostoyevsky"
  , "Fyodor Dostoevsky"
  , "Fyodor Dostoevsky"
  , "Fyodor Dostoevski"
  , "Fyodor Dostoevskij"
  , "Fyodor Dostoevskii" 
  , "Fyodor Dostoyevski"
  , "Fyodor Dostoyevskij"
  , "Fyodor Dostoevsky"
  , "Dostoyevsky"
  , "Dostoevski"
  , "Dostoevskij"
  , "Dostoevskii" 
  , "Dostoyevsky"
  , "Dostoyevski"
  , "Dostoyevskij"
  , "Dostoyevskii"
  , "Dostoyevskii"
  ]

dostoevsky :: [T.Text]
dostoevsky = ["Fyodor Dostoevsky"]

tolstoys :: [T.Text]
tolstoys = ["Leo Tolstoy", "Count Leo Tolstoy", "Tolstoy", "Tlostoy"]

tolstoy :: [T.Text]
tolstoy = ["Leo Tolstoy"]

all' :: [T.Text]
all' = dostoevskys ++ tolstoys ++ eliots ++ cowards

rate :: T.Text -> [T.Text] -> [(Int, T.Text)]
rate canonical = fmap $ \w -> (textDiff canonical w, w)

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


tagNestedEntriesByAuthPred :: (Author -> Bool) -> [LogEntry] -> [[LogEntry]]
tagNestedEntriesByAuthPred authPred [] = []
tagNestedEntriesByAuthPred authPred (x:xs)
  | doesAuthorSatisfy authPred x =
    case x of
      TabTsEntry (tabs, _, _) ->
        let (belong, rest) = takeWhileRest (doesEntryBelongToParent tabs) xs
        in belong : tagNestedEntriesByAuthPred authPred rest
      Dump _ -> [] -- this case sholud NEVER occur
  | otherwise = tagNestedEntriesByAuthPred authPred xs


-- | Where there is neither a title predicate nor an author predicate, applies
-- only variant (a.t.m. def and quote) filters; otherwise, where there is at
-- least one `Read` predicate, filters by nesting and applies variant filters.
filterWith :: Input -> [LogEntry] -> [LogEntry]
filterWith _ [] = []
filterWith input l@(x:xs)
  -- no search predicates
  | isNothing (titlePred input) && isNothing (authorPred input) = isRequested input $ l
  -- toplevel read entry, essentially tags nested entries with auth/title attr;
  -- these are collected
  | doesReadSatisfy input x =
    case x of
      TabTsEntry (tabs, _, _) ->
        let (belong, rest) = takeWhileRest (doesEntryBelongToParent tabs) xs
         in (x: isRequested input belong) ++ filterWith input rest
  -- collect toplevel quotes that satisfy
  | isTopLevel x && satisfies input x = x : filterWith input xs
  | otherwise = filterWith input xs

isTopLevel :: LogEntry -> Bool
isTopLevel (Dump _) = True
isTopLevel (TabTsEntry (tabs, _, _))
  | tabs == 0 = True
  | otherwise = False

-- | Determines whether an entry satisfies the search predicates specified by a
-- given `Input`.
satisfies :: Input -> LogEntry -> Bool
satisfies (Input _ _ ap tp _ _) le =
  let res =
        projectQuotation le >>=
        return .
        (\(q, attr, _) ->
           case (ap <*> pure attr, tp <*> pure attr) of
             (Just t, Just a) -> t && a
             (Just t, Nothing) -> t
             (Nothing, Just a) -> a
             (Nothing, Nothing) -> True)
  in case res of
       Just b -> b
       Nothing -> False

-- | Filters out entries not of the requested type (requests received via
-- --definitions and --quotations flags). 
isRequested :: Input -> [LogEntry] -> [LogEntry]
isRequested (Input _ _ _ _ True True) = filter (\l -> isDef l || isQuote l) -- get defs and quotes
isRequested (Input _ _ _ _ True False) = filter isDef -- get defs
isRequested (Input _ _ _ _ False True) = filter isQuote -- get quotes
isRequested (Input _ _ _ _ False False) = id -- get everything

isDef :: LogEntry -> Bool
isDef = isJust . projectDefQuery

isQuote :: LogEntry -> Bool
isQuote = isJust . projectQuotation
                                
takeWhileRest :: (a -> Bool) -> [a] -> ([a], [a])
takeWhileRest pred [] = ([],[])
takeWhileRest pred (x:xs) 
  | pred x = go x $ takeWhileRest pred xs
  | otherwise = ([], x:xs)
  where go x (as, bs) = (x:as, bs)

doesAuthorSatisfy :: (Author -> Bool) -> LogEntry -> Bool
doesAuthorSatisfy authPred (TabTsEntry (_, _, (Read _ author))) = authPred author
doesAuthorSatisfy _ _ = False

doesTitleSatisfy :: (Title -> Bool) -> LogEntry -> Bool
doesTitleSatisfy titlePred (TabTsEntry (_, _, (Read title _))) = titlePred title
doesTitleSatisfy _ _ = False

-- | For each present read predicate (a.t.m. only on author and title strings)
-- apply both to `LogEntry` if it's `Read` of `Entry`.
doesReadSatisfy :: Input -> LogEntry -> Bool
doesReadSatisfy (Input _ _ ap tp _ _) le = 
  case getEntry le >>= getRead of
    Just (t, a) -> case (tp <*> pure t, ap <*> pure a) of
                     (Just t, Just a) -> t && a
                     (Just t, Nothing) -> t
                     (Nothing, Just a) -> a
                     (Nothing, Nothing) -> True -- w/o predicates validate all reads
    Nothing -> False
   --   x = fmap first tp <*> (fmap second ap <*> pair)


-- | Takes parent (read entry's) indentation level, a log entry
doesEntryBelongToParent :: Int -> LogEntry -> Bool
doesEntryBelongToParent tabs (Dump _) = False -- breaks on dumps
doesEntryBelongToParent tabs (TabTsEntry (tabs', _, _))
  | tabs' > tabs = True
  | otherwise = False

-- | Takes between first two successes of element predicate. The first element
-- to satisfy is included, the second is not. The rest of the list is the
-- second element of the returned tuple. Elements before the the first element
-- to satisfy are discarded. E.g.,
--
-- >>> takeUntil (> 2) [0, 1, 3, 0, 2, -1, 4, 6, 7]
-- ([3, 0, 2, -1], [4, 6, 7])
takeFromUntil :: (a -> Bool) -> [a] -> ([a], [a], [a])
takeFromUntil pred = breakAgain . break pred
  where
    breakAgain (ys, []) = (ys, [], [])
    breakAgain (ys, (x:xs)) =
      let (span, rest) = (x :) <$> break pred xs
      in (ys, span, rest)

testLogWithDumpOutput :: [LogEntry]
testLogWithDumpOutput =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 55, sec = 17}
      , Def
          (DefVersus
             "benignant"
             "kind; gracious; favorable;"
             "benign"
             "gentle, mild, or, medically, non-threatening"))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 45}
      , Def
          (DefVersus
             "malignant"
             "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
             "malign"
             "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Def (Defn (Just 38) ["inimical", "traduce", "virulent"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "Her simplicity fathomed what clever people falsified."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

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
project (TabTsEntry (_, _, (Def dq))) = Just dq
project _ = Nothing



projectDef :: LogEntry -> [(Headword, Maybe Meaning)]
projectDef (TabTsEntry (_, _, (Def dq))) =
  case dq of
    Defn mPg headwords -> flip (,) Nothing <$> headwords
    InlineDef hw meaning -> [(hw, Just meaning)]
    DefVersus hw m hw' m' -> [(hw, Just m), (hw', Just m')]
projectDef _ = []
