{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
   OverloadedStrings #-}
module Main where

import Lib
import Parse
import Helpers
import Search


import Prelude hiding (lookup, log)
import Control.Monad (void)
import Data.Aeson hiding (Null)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, sort, intercalate)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Options.Applicative
import qualified Text.Trifecta.Result as Tri
import Data.Yaml.Config (load, lookup, lookupDefault, subconfig)
import System.Directory (doesFileExist, createDirectoryIfMissing, listDirectory)
import System.Environment (getEnv)

-- Questions:
--
-- * default date span?
-- * 
-- Flags:
--
-- * --after DATE, --before DATE: month, day, year
--
--   N.B: --since, --until are valid alternatives
--
--    * DATE:
--
--        - configurable date format. my personal default is day/month/year
--        - when a date attribute, e.g., day, month, or year, is omitted, the
--          that of the current date is used
--
--        - relative dates: 
--
--           --relative <rdate>
--           <rdate> : (order agnostic, all but one field is optional) 'NNdNNmNNy', 
--                     e.g., '1m', '2m3d', '3d1m2y'.
--
--
--
-- * --filter (author | title | definition)
--
-- muse [(--within | -w) relDATE ] [(--author | -a) AUTHOR] 
--      ([(--title | -t) TITLE] [--definitions | -d] | [(--quotation | -q) [SEARCH_STRING]])
--
-- For instance, 'muse -w 5d3m2y'
-- TODO
-- □  relative date parser
-- | Reads `RelDur`.
-- (order agnostic, all but one field is optional) 'NNdNNmNNy', 
--          e.g., '1m', '2m3d', '3d1m2y'.
--
-- N.B.: ordered parsing on first pass
relDurReader :: ReadM RelDur
relDurReader =
  eitherReader $ \s ->
    case parse relDur s of
      Tri.Success rd -> Right rd
      Tri.Failure err ->
        Left $
        "Cannot parse relative date by (dmy): " ++
        s ++ "\nErrInfo: " ++ show err
  -- d m y
  -- y m d
  -- m d y

-- | Contains filters, either entry predicates or projections, including:
--
-- * filter by author match
-- * filter by title match
-- * filter within date
-- * map definition extraction 
-- * map quotation extraction
--
-- Of the above, filters will be applied (in arbitrary order, a.t.m.) to log
-- entries, after which map will be applied to the remainder.
--
-- A `Search` instance will be generated by the 
data Search a = Search
  { filters :: [(Entry -> Bool)]
  , map :: Entry -> a
  }

data DateTime =
  DateTime


-- | Convert duration, combined with the system time, into UTC time. See the
-- `time` library.
--
-- TODO: handle d/m/y excess w/ rollover
subRelDur :: Day -> RelDur -> Day
subRelDur day (RelDur y m d) =
  addGregorianYearsRollOver (negate y) .
  addGregorianMonthsRollOver (negate m) . addDays (negate d) $
  day

search :: Day -> Parser (Input SearchResult)
search today = Input <$> (subRelDur today 
                     <$> within) 
                     <*> pure today 
                     <*> (isInfixOf <$> author)
                     <*> (isInfixOf <$> title)
                     <*> defs
                     <*> quotes

defs :: Parser Bool
defs = switch $ long "definitions" 
  <> short 'd' 
  <> help "Collect definitions of left-over entries."

quotes :: Parser Bool
quotes = switch $ long "quotations" 
  <> short 'q' 
  <> help "Collect quotations of remaining entries."

entryToSearchResult :: Entry -> SearchResult
entryToSearchResult (Def dq) = Def' (show dq)
entryToSearchResult (Quotation b attr pg) = Quotation' b attr pg
entryToSearchResult (Read t a) = Read' t a
entryToSearchResult (Commentary s) = Commentary' s
entryToSearchResult (PN pg) = PN' pg
entryToSearchResult Null = Null'

author :: Parser String
author =
  strOption
    (long "author" 
    <> metavar "SUBSTR"
    <> short 'a' <> value "" <> help "Substring/affix of author")

title :: Parser String
title =
  strOption 
    (long "title" 
    <> metavar "SUBSTR"
    <> short 't' <> value "" <> help "Affix of title")

data Sample = Sample
  { hello :: String
  , quiet :: Bool
  , enthusiasm :: Int
  , rd :: RelDur
  }

sample :: Parser Sample
sample =
  Sample <$>
  strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting") <*>
  switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
  option
    auto
    (long "enthusiasm" <> help "How enthusiastically to greet" <> showDefault <>
     value 1 <>
     metavar "INT") <*>
  option
    relDurReader
    (long "within" <> help "How far back within logs to apply search filters" <>
     short 'w' <>
     value (RelDur 0 6 0))

within :: Parser RelDur
within =
  option
    relDurReader
    (long "within" 
    <> metavar "REL_DATE"
    <> help "Lower bound of search filter range" <>
     short 'w' <>
     value (RelDur 0 6 0))

main' :: IO ()
main' =  loadMuseConf >>= parseAllEntries
    
main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  let opts =
        info
          (search today <**> helper)
          (fullDesc <> progDesc "Run logParse search filters." <>
           header "muse - a reading log search interface")
  execParser opts >>= runSearch 

runSearch :: Input SearchResult -> IO ()
runSearch (Input s e _ _ dfs qts) = do
  let dateFilter = do
        mc <- loadMuseConf
        fps <- listDirectory . T.unpack $ entryCache mc
        filterBy s e <$> pathsToDays fps
  filtered <- dateFilter
  putStrLn $
    "start date: " ++
    show s ++
    "\n" ++
    "end date: " ++
    show e ++
    "\ncollect defs: " ++
    show dfs ++
    "\ncollect quotations: " ++
    show qts ++
    "\nfancy search magick!" ++
    "\nfiltered dates (to be searched):\n" ++ show filtered


-- config
data MuseConf = MuseConf
  { log :: T.Text
  , cache :: T.Text
  , home :: T.Text
  } deriving (Eq, Show)

entryCache :: MuseConf -> T.Text
entryCache (MuseConf _ cache _) = cache <> "/parsedEntries"

loadMuseConf :: IO MuseConf
loadMuseConf = do
  config <- load "./config.yaml"
  home' <- getEnv "HOME"
  let home = T.pack home'
      -- defaults
      logDef = home <> "/.muse/entries/"
      cacheDef = home `T.append` "/.cache/muse/"
      -- lookup
      logDir = lookupDefault "log-dir" logDef config
      cacheDir = lookupDefault "cache-dir" cacheDef config
  mapM_ T.putStrLn ["muse config: ", logDir, cacheDir]
  return $ MuseConf logDir cacheDir home

writeMuseConf :: MuseConf -> IO MuseConf
writeMuseConf mc@(MuseConf log cache home) = do
  let conf = "log-dir: " <> log <> "\n\ncache-dir: " <> cache
  createDirectoryIfMissing True $ T.unpack  (home <> "/.muse")
  T.writeFile (T.unpack $ home <> "/.muse/config.yaml") conf
  return mc


-- muse init:
-- 1. prompt for config file creation, default at ~/.muse
-- 2. check whether (a) logDir and (b) cacheDir exist.
--    i.  if both (a) and (b) hold, then proceed to (3)
--    ii. otherwise prompt for missing locations, write choices to ~/.muse
-- 3. parse log dir; serialize successful results ([Int, TS, Entry]) to
--    cacheDir/entries (bucket by month); collect filenames of parse failures 
--    into cacheDir/failures. 
--
--    -  write valid date range to config ?
--    TODO tag generation based on entry grouping
-- 4. 


-- | Prompt user for log dir, cache dir, 
prompt :: IO MuseConf
prompt = do
  home' <- getEnv "HOME"
  let home = T.pack home'
      defConfPath = home <> "/.muse"
      defCacheDir = home <> "/.cache/muse"
  -- 1. prompt for config file creation, default at ~/.muse
  --T.putStr $ "Enter path to config directory (default: " <> defConfPath <> "): "
  --resp <- T.getLine
  --let confPath :: T.Text
  --    confPath =
  --      case resp of
  --        "" -> defConfPath
  --        _ -> resp
  T.putStr $ "Enter path to entry directory (default: " <> defConfPath <> "/entries/): "
  resp <- T.getLine
  let entryDir :: T.Text
      entryDir =
        case resp of
          "" -> defConfPath <> "/entries"
          _ -> resp
  T.putStr $ "Enter path to cache directory (default: " <> defCacheDir <> "): "
  resp <- T.getLine
  let cacheDir =
        case resp of
          "" -> defCacheDir
          _ -> resp
      conf = MuseConf entryDir defConfPath cacheDir
  writeMuseConf conf



-- | Creates ~/.muse/{entries/,config.yaml} and ~/.cache/muse/entries.
museInit :: IO MuseConf
museInit = do
  home' <- getEnv "HOME"
  let home = T.pack home'
      defConfPath = home <> "/.muse/config.yaml"
      defCacheDir = home <> "/.cache/muse/"
      defLogDir = home <> "/.muse/entries/"
      defaults = MuseConf defLogDir defCacheDir home
  T.putStrLn $ "Config file located at " <> home <> "/.muse/config.yaml"
  -- create config & conf dir
  mc <- writeMuseConf defaults
  createDirectoryIfMissing True . T.unpack $ (cache mc) <> "/parsedEntries"
  createDirectoryIfMissing True $ T.unpack (log mc)
  parseAllEntries mc
  return mc

-- | Parse all entries from logDir into cacheDir/entries.
parseAllEntries :: MuseConf -> IO ()
parseAllEntries mc@(MuseConf log cache home) = do
  -- read in log file names; parse 'em
  putStrLn $ show mc
  fps <- sort <$> listDirectory (T.unpack log)
  let --parse' :: String -> IO (String, Either String [(Int, TimeStamp, Entry)])
      --parse' fp =
      --  (,) <$> pure fp <*>
      --  (showErr . parseByteString entries mempty <$> B.readFile (T.unpack log ++ "/" ++ fp))

      parse :: String -> IO (String, Either String [LogEntry])
      parse fp =
        (,) <$> pure fp <*>
        (showErr . parseByteString logEntries mempty <$> B.readFile (T.unpack log ++ "/" ++ fp))


      invert :: (fp, Maybe e) -> Maybe (fp, e)
      invert =
        \(fp, me) ->
          case me of
            Just e -> Just (fp, e)
            Nothing -> Nothing

      --parseAndShowErrs' :: IO [(String, [(Int, TimeStamp, Entry)])]
      --parseAndShowErrs' = sequence (parse <$> fps) >>= showOrCollect

      parseAndShowErrs :: IO [(String, [LogEntry])]
      parseAndShowErrs = sequence (parse <$> fps) >>= showOrCollect

      showOrCollect :: [(String, Either String res)] -> IO [(String, res)]
      showOrCollect =
        let sideBar = unlines . fmap ("> " ++) . lines
        in foldr
             (\(fp, e) rest ->
                case e -- render errros w filename, `ErrInfo`
                      of
                  Left err -> putStrLn ("File: " ++ fp ++ "\n" ++ sideBar err) >> rest
                  Right res -> ((fp, res) :) <$> rest)
             (return [])

  entryGroups <- parseAndShowErrs
  sequence_ $ fmap (\(fp, eg) -> BL.writeFile (T.unpack cache ++ "/parsedEntries/" ++ fp) (encode eg)) entryGroups

-- TODO 
--
-- □  centralize file path generation--save yourself the headache later of
--    mismatched paths!
-- on parse failure, show user err info
--
parsedEntryDir :: MuseConf -> T.Text
parsedEntryDir = undefined
