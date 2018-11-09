{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
   OverloadedStrings, MultiWayIf #-}
module Search where

import           Control.Monad (void)
import           Data.Aeson hiding (Null)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.List (isInfixOf, sort, intercalate)
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Metrics (levenshtein)
import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Clock (utctDay)
import           Data.Yaml.Config (load, lookup, lookupDefault, subconfig)
import           Helpers
import           Parse
import           Prelude hiding (lookup, log, min)
import           System.Directory (doesFileExist, createDirectoryIfMissing, listDirectory)
import           System.Environment (getEnv)
import           Text.Show.Pretty (pPrint)
import qualified Text.Trifecta.Result as Tri

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



filterBy :: Day -- ^ start date (lower bound)
         -> Day -- ^ end date (upper bound)
         -> [Day] 
         -> [Day]
filterBy l u = filter (\d -> d >= l && d <= u) 

genEntryPred :: Input SearchResult -> (LogEntry -> Bool)
genEntryPred (Input _ _ auth title _ _) (TabTsEntry (tab, ts, ent)) = undefined


type Tag = String
type TagPred = String -> Bool -- | Tag predicate.
-- TODO
-- Crawls a `[LogEntry]` and applies to indented/nested (depth>=1) entries a
-- tag generated from the first-level parent.
tagIndented :: [LogEntry] -> [(LogEntry, [Tag])]
tagIndented = undefined
-- TODO
-- | Generate a tag for an entry; succeeds for `Read` variant.
--
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


