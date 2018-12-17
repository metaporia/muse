{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main (muse)
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the necessaries for muse's CLI.
--
-- TODO 
--
-- ▣  persist with "Data.Acid"
--
-- ▣  serialize w "Data.Serialize" and "SafeCopy"
--
-- □  include locale time zone per file. for midday timezone change, create
--    extra log file for each, with special name. add name handling .
--
-- □  convert inline defs, phrases with meanings to anki card *.txt format
--    □  vim helper: generate cloze from selected quote
--
-----------------------------------------------------------------------------
module Main where

import Control.Exception (bracket)
import Control.Monad ((>=>), join, void)
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Remote
import Data.Aeson hiding (Null)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (fold)
import Data.List
       (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Monoid ((<>))
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Data.Yaml.Config (load, lookup, lookupDefault, subconfig)
import Helpers
import Lib
import Options.Applicative
import Parse
import Parse.Entry
import Prelude hiding (init, log, lookup)
import Render
import Search
import Store hiding (Author, Search, Search', Title, defs, quotes)
import qualified Store
import Store.Render
import System.Directory
       (createDirectoryIfMissing, doesFileExist, getModificationTime,
        listDirectory)
import System.Environment (getEnv)
import Text.Show.Pretty (pPrint)
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Result as Tri

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

-- | attach after --author and --title flags before search string. the search 
-- mode and the search string are separated by a space
searchPredOpt :: String -> Either String (SearchType, String) -- search type, rest of string
searchPredOpt s =
  case parse ((,) <$> searchType <*> Tri.many Tri.anyChar) s of
    Tri.Success rd -> Right rd
    Tri.Failure err ->
      Left $
      "Cannot parse search predicate options: " ++
      s ++ "\nErrInfo: " ++ show err

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
data DateTime =
  DateTime

-- | Convert duration, combined with the system time, into UTC time. See the
-- `time` library.
subRelDur :: Day -> RelDur -> Day
subRelDur day (RelDur y m d) =
  addGregorianYearsRollOver (negate y) .
  addGregorianMonthsRollOver (negate m) . addDays (negate d) $
  day

-- | Represents filters and entry maps extracted from CLI invocation.
-- Ugly applicative-do parse error workaround--it couldn't handle the input
-- conversion inside a do block.
data TmpInput = TmpInput
  { startDateTime :: Day
  , endDateTime :: Day
  , authorPred :: Maybe (Author -> Bool)
  , titlePred :: Maybe (Title -> Bool)
  , predicates :: [Maybe (LogEntry -> Bool)]
  }

searchTmp :: Day -> Parser TmpInput
searchTmp today = do
  w <- (subRelDur today <$> within)
  ds <- defs
  qs <- quotes
  ps <- phrases'
  dias <- dialogues'
  a <- fmap consumeSearchType <$> author
  t <- fmap consumeSearchType <$> title
  pure (TmpInput w today a t [ds, qs, ps, dias])

search' :: Day -> Parser Store.Search
search' today = do
  attrs <- (<>) <$> authorS <*> titleS
  s <- (subRelDur today <$> since)
  dhw <- (fmap . fmap) (isInfixOf) defHW
  dm <- (fmap . fmap) (isInfixOf) defMeaning
  q <- (fmap . fmap) (T.isInfixOf . T.pack) quoteBody
  phw <- (fmap . fmap) isInfixOf phraseHW
  pm <- (fmap . fmap) isInfixOf phraseMeaning
  dias <- (fmap . fmap) (T.isInfixOf . T.pack) dialogueBody
  comments <- (fmap . fmap) (T.isInfixOf . T.pack) commentBody
  dumps <- (fmap . fmap) (T.isInfixOf . T.pack) dumpBody
  return
    (Store.Search
       s
       today
       []
       (BucketList dumps (dhw, dm) [] q dias (phw, pm) comments))

toInput :: TmpInput -> Input
toInput (TmpInput s e ap tp preds) =
  let ap' = flip guardStrSearch preds . convertAuthSearch <$> ap
      tp' = flip guardStrSearch preds . convertTitleSearch <$> tp
  in Input s e ap' tp' preds

search :: Day -> Parser Input
search d = toInput <$> searchTmp d

dispatchSearchType :: Eq a => SearchType -> [a] -> [a] -> Bool
dispatchSearchType Prefix = isPrefixOf
dispatchSearchType Infix = isInfixOf
dispatchSearchType Suffix = isSuffixOf

-- | Defaults to infix search on whole search string, otherwise parses search
-- mode, a single space, and the remainder is passed as the search string to
-- author/title predicate
consumeSearchType :: String -> (String -> Bool)
consumeSearchType s =
  case eitherToMaybe (searchPredOpt s) of
    Just (fix, searchStr) -> dispatchSearchType fix searchStr
    Nothing -> isInfixOf s

data InputType
  = File FilePath
  | StdIn String
  | All -- ^ Parse all entries in `entrySource`
     Bool -- ^ Silence errors?
     Bool -- ^ Ignore parsed entry cache?
  deriving (Eq, Show)

-- | A bare invocation will default to `parse --all --ignore-cache`, which
-- parses all entries, uses parsed entry cache, and displays parse errors.
parse' :: Parser InputType
parse' =
  File <$> fileInput <|> StdIn <$> stdInput <|> allEntries <|>
  pure (All False False)

allEntries :: Parser InputType
allEntries =
  (\_ s i -> All s i) <$>
  switch
    (long "all" <> short 'a' <>
     help "Parse all entries in 'log-dir' (see ~/.muse/config.yaml)") <*>
  quiet <*>
  ignore

stdInput :: Parser String
stdInput =
  option
    str
    (metavar "LOG_CONTENT" <> long "stdin" <> short 's' <>
     help "Parse from stdin") <**>
  helper

fileInput :: Parser FilePath
fileInput =
  option
    str
    (long "file" <> metavar "FILE" <> help "Read in log from file" <> short 'f') <**>
  helper

init :: Parser SubCommand
init = Init <$> quiet <*> ignore

init' :: Parser SubCommand'
init' = Init' <$> quiet <*> ignore

quiet :: Parser Bool
quiet =
  switch
    (long "quiet" <> short 'q' <> help "Suppress log parser error messages")

ignore :: Parser Bool
ignore =
  switch
    (long "ignore-cache" <> short 'i' <>
     help "Reparse all entries, ignore cache (overwrites cache)")

color :: Parser Bool
color =
  switch (long "color" <> short 'c' <> help "Colorize output; off by default")

toplevel' :: Day -> Parser Opts'
toplevel' today =
  Opts' <$> color <*>
  subparser
    (command
       "search"
       (info
          (Search' <$> search' today <**> helper)
          (progDesc
             "Search log entries by date, author, title, or predicate on entry contents.\
             \ Inline definitions of headwords or phrases can be searched as well.")) <>
     command
       "parse"
       (info
          (Parse' <$> parse' <**> helper)
          (progDesc "Parse entries; a bare invocation runs 'parse --all'")) <>
     command
       "lint"
       (info
          (pure Lint')
          (progDesc "TBD; for, e.g., author attribution validation")) <>
     command
       "init"
       (info
          (init' <**> helper)
          (progDesc
             "Initialize config file, cache directory, and entry log\
             \ directory; parse all entries in 'log-dir'")))

data SubCommand'
  = Search' Store.Search
  | Parse' InputType
  | Lint'
  | Init' Bool
          Bool

data Opts' = Opts'
  { colorize' :: Bool
  , subcommand' :: SubCommand'
  }

toplevel :: Day -> Parser Opts
toplevel today =
  Opts <$> color <*>
  subparser
    (command
       "search"
       (info
          (Search <$> search today <**> helper)
          (progDesc
             "Search log entries by date, author, title, or predicate on entry contents.\
             \ Inline definitions of headwords or phrases can be searched as well.")) <>
     command
       "parse"
       (info
          (Parse <$> parse' <**> helper)
          (progDesc "Parse entries; a bare invocation runs 'parse --all'")) <>
     command
       "lint"
       (info
          (pure Lint)
          (progDesc "TBD; for, e.g., author attribution validation")) <>
     command
       "init"
       (info
          (init <**> helper)
          (progDesc
             "Initialize config file, cache directory, and entry log\
             \ directory; parse all entries in 'log-dir'")))

-- | Stores options applicable to all subcommands.
data Opts = Opts
  { colorize :: Bool
  , subcommand :: SubCommand
  }

data SubCommand
  = Search Input
  | Parse InputType
  | Lint
  | Init Bool -- ^ Suppress log parse errors
         Bool -- ^ Reparse cached entries

defs :: Parser (Maybe (LogEntry -> Bool))
defs =
  flag Nothing (Just isDef) $
  long "definitions" <> short 'd' <> help "Collect only definitions"

-- beginning of rewrite
defHW :: Parser [String]
defHW =
  option (either (const []) id . parsePreds <$> str) $
  long "definitions" <> short 'd' <>
  help "Collect only satisfactory definitions"

defMeaning :: Parser [String]
defMeaning =
  option (either (const []) id . parsePreds <$> str) $
  long "dm" <> long "def-meaning" <>
  help "Search for strings within meaning/definition."

phraseHW :: Parser [String]
phraseHW =
  option (either (const []) id . parsePreds <$> str) $
  long "phrases" <> short 'p' <> help "Collect only satisfactory phrases"

phraseMeaning :: Parser [String]
phraseMeaning =
  option (either (const []) id . parsePreds <$> str) $
  long "pm" <> long "phr-meaning" <>
  help "Search for strings within meaning/definition."

quoteBody :: Parser [String]
quoteBody =
  option (either (const []) id . parsePreds <$> str) $
  long "quote" <> short 'q' <> help "Collect satisfactory quotes."

dialogueBody :: Parser [String]
dialogueBody =
  option (either (const []) id . parsePreds <$> str) $
  long "dialogue" <> long "dia" <> help "Collect satisfactory dialogues."

commentBody :: Parser [String]
commentBody =
  option (either (const []) id . parsePreds <$> str) $
  long "comment" <> short 'c' <> help "Collect satisfactory comments."

dumpBody :: Parser [String]
dumpBody =
  option (either (const []) id . parsePreds <$> str) $
  long "dump" <> help "Collect satisfactory dumps."

-- end of rewrite
authorS :: Parser [String]
authorS =
  option (either (const []) id . parsePreds <$> str) $
  long "author" <> short 'a' <>
  help "Collect entries attributed to satisfactory authors."

-- end of rewrite
titleS :: Parser [String]
titleS =
  option (either (const []) id . parsePreds <$> str) $
  long "title" <> short 't' <>
  help "Collect entries attributed to satisfactory authors."

-- | Parse caret separated list of strings.
parsePreds :: String -> Either String [String] -- search type, rest of string
parsePreds s =
  case parse preds s of
    Tri.Success rd -> Right rd
    Tri.Failure err ->
      Left $
      "Cannot parse '^' separated predicate list: " ++
      s ++ "\nErrInfo: " ++ show err

quotes :: Parser (Maybe (LogEntry -> Bool))
quotes =
  flag Nothing (Just isQuote) $
  long "quotations" <> short 'q' <> help "Collect only quotations"

phrases' :: Parser (Maybe (LogEntry -> Bool))
phrases' =
  flag Nothing (Just isPhrase) $
  long "phrases" <> short 'p' <> help "Collect only phrases"

dialogues' :: Parser (Maybe (LogEntry -> Bool))
dialogues' =
  flag Nothing (Just isDialogue) $
  long "dialogues" <> short 'l' <> help "Collect only dialogue"

author :: Parser (Maybe String)
author =
  option
    (fmap Just str)
    (long "author" <> metavar "SUBSTR" <> short 'a' <> value Nothing <>
     help "Substring/affix of author")

title :: Parser (Maybe String)
title =
  option
    (fmap Just str)
    (long "title" <> metavar "SUBSTR" <> short 't' <> value Nothing <>
     help "Affix of title")

within :: Parser RelDur
within =
  option
    relDurReader
    (long "within" <> metavar "REL_DATE" <>
     help "Lower bound of search filter range" <>
     short 'w' <>
     value (RelDur 0 6 0))

since :: Parser RelDur
since =
  option
    relDurReader
    (long "since" <> metavar "REL_DATE" <>
     help "Lower bound of search filter range" <>
     short 's' <>
     value (RelDur 0 6 0))

showDebug = False

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  execParser (info (helper <*> toplevel today) (fullDesc <> header "dispatch")) >>=
    dispatch

main' :: IO ()
main' = do
  today <- utctDay <$> getCurrentTime
  execParser (info (helper <*> toplevel' today) (fullDesc <> header "dispatch")) >>=
    dispatch'

dispatch' :: Opts' -> IO ()
dispatch' opts@(Opts' color (Search' s)) = do
  bracket
    (openLocalStateFrom "state/DB" initDB)
    createCheckpointAndClose
    (\acid -> do
       db <- query acid ViewDB
       putStrLn "searching...\n"
       runSearch' showDebug color s db
       return ())
dispatch' (Opts' color (Lint')) = putStrLn "linting"
dispatch' (Opts' color (Init' quiet ignoreCache)) = do
  putStrLn "initializing...\n" -- ++ showMuseConf mc
  void $ museInit quiet ignoreCache
dispatch' (Opts' color (Parse' it)) = do
  mc <- loadMuseConf
  putStrLn "parsing..."
  s <-
    case it of
      File fp -> readFile fp
      StdIn s -> return s
      -- TODO update 'parseAllEntries'
      All silence ignore -> parseAllEntries silence ignore mc >> return ""
  putStrLn s

ifNotNull l true false =
  if not (null l)
    then true
    else false

-- | Pretty print output of bucket filters.
runSearch' :: Bool -> Bool -> Store.Search -> DB -> IO ()
runSearch' debug color search db@(DB dmp defs rds qts dias phrs cmts _)
  -- FIXME: check for null predicate lists
 = do
  colRender color $
    join
      [ filterDumps search dmp
      , filterDefs db search defs
      , filterQuotes db search qts
      , filterDialogues db search dias
      , filterPhrases db search phrs
      , filterComments db search cmts
      ]

dispatch :: Opts -> IO ()
dispatch opts@(Opts color (Search inp)) =
  putStrLn "searching...\n" >> runSearch showDebug color inp
dispatch (Opts color (Lint)) = putStrLn "linting"
dispatch (Opts color (Init quiet ignoreCache)) = do
  putStrLn "initializing...\n" -- ++ showMuseConf mc
  void $ museInit quiet ignoreCache
dispatch (Opts color (Parse it)) = do
  mc <- loadMuseConf
  putStrLn "parsing..."
  s <-
    case it of
      File fp -> readFile fp
      StdIn s -> return s
      -- TODO fix unintuitive/broken parse CLI behavior
      -- TODO `muse parse` should run `--update` by default, using cached logs
      -- where the source file is unchanged
      All silence ignore -> parseAllEntries silence ignore mc >> return ""
  putStrLn s

-- | As yet, this searches only pre-parsed `LogEntry`s.
runSearch :: Bool -> Bool -> Input -> IO ()
runSearch debug colorize input@(Input s e tp ap preds) = do
  let dateFilter = do
        mc <- loadMuseConf
        fps <- listDirectory . T.unpack $ entryCache mc
        dates <- sort . filterBy s e <$> pathsToDays fps
        let cachePath = (T.unpack (entryCache mc) ++)
            entries =
              loadFiles (cachePath . dayToPath <$> dates) >>=
              return . catMaybes . decodeEntries
              -- TODO print date above each days `[LogEntry]`
            filtered =
              concat . fmap (rmOnlyRead . filterWith' input) <$> entries
              where
                rmOnlyRead xs
                  | M.getAll $ foldMap (M.All . isRead) xs = []
                  | otherwise = xs
        return filtered
  filtered <- join dateFilter
  if debug
    then putStrLn
           ("start date: " ++
            show s ++
            "\n" ++
            "end date: " ++
            show e ++ "\nfancy search magick!" ++ "colors?: " ++ show colorize) >>
         pPrint (filterWith' input testLogWithDumpOutput)
    else return ()
  putStrLn "predicates:"
  sequence_ . fmap (colRender colorize) $ filtered
  return ()

-- config
data MuseConf = MuseConf
  { log :: T.Text -- ^ * "$HOME/.muse/entries
  , cache :: T.Text
  , home :: T.Text
  } deriving (Eq, Show)

entryCache :: MuseConf -> T.Text
entryCache (MuseConf _ cache _) = cache <> "parsedEntries/"

entrySource :: MuseConf -> T.Text
entrySource = log

config :: MuseConf -> T.Text
config mc = home mc <> "/.muse/config.yaml"

-- | Expects config at  $HOME/.muse/config.yaml
loadMuseConf :: IO MuseConf
loadMuseConf = do
  home' <- getEnv "HOME"
  config <- load $ home' ++ "/.muse/config.yaml"
  let home = T.pack home'
      logDef = home <> "/.muse/entries/"
      cacheDef = home `T.append` "/.cache/muse/"
      -- lookup
      logDir = lookupDefault "log-dir" logDef config
      cacheDir = lookupDefault "cache-dir" cacheDef config
  mapM_ T.putStrLn ["muse config: ", logDir, cacheDir]
  return $ MuseConf logDir cacheDir home

showMuseConf :: MuseConf -> String
showMuseConf = show

writeMuseConf :: MuseConf -> IO MuseConf
writeMuseConf mc = do
  let conf =
        "log-dir: " <> (entrySource mc) <> "\n\ncache-dir: " <> entryCache mc
  createDirectoryIfMissing True $ T.unpack (home mc <> "/.muse")
  T.writeFile (T.unpack $ config mc) conf
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
--    - write valid date range to config ?
--    - tag generation based on entry grouping
-- 4. 
-- | Prompt user for log dir, cache dir, 
prompt :: IO MuseConf
prompt = do
  home' <- getEnv "HOME"
  let home = T.pack home'
      defConfPath = home <> "/.muse"
      defCacheDir = home <> "/.cache/muse"
  T.putStr $
    "Enter path to entry directory (default: " <> defConfPath <> "/entries/): "
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
--
--  Checks whether the following exist before mutation:
--      
--      ▣  config file, `config.yaml`
--      □  `.muse{,entries}`
--      □  `.cache/muse{,parsedEntries}`
--
museInit :: Bool -> Bool -> IO MuseConf
museInit quiet ignoreCache = do
  home' <- getEnv "HOME"
  let home = T.pack home'
      defConfPath = home <> "/.muse/config.yaml"
      defConfPath' = T.unpack defConfPath
      defCacheDir = home <> "/.cache/muse/"
      -- FIXME
      stateDir = home <> "/.muse/state"
      defLogDir = home <> "/.muse/entries/"
      defaults = MuseConf defLogDir defCacheDir home
  T.putStrLn $ "Expects configuration file at: " <> home <> defConfPath <> "\n"
  -- create config & conf dir
  doesExist <- doesFileExist defConfPath'
  mc <-
    if doesExist
      then loadMuseConf
      else writeMuseConf defaults
  createDirectoryIfMissing True . T.unpack $ entryCache mc -- (cache mc) <> "/parsedEntries"
  T.putStrLn $ entryCache mc <> " and " <> log mc <> " found or created."
  createDirectoryIfMissing True $ T.unpack (log mc)
  parseAllEntries quiet ignoreCache mc
  return mc

-- | List file names in log source directory
lsEntrySource :: MuseConf -> IO [FilePath]
lsEntrySource = listDirectory . T.unpack . entrySource

-- | List file names (per day) of parsed `LogEntry`s.
lsEntryCache :: MuseConf -> IO [FilePath]
lsEntryCache = listDirectory . T.unpack . entryCache

-- | Load contents at file paths.
loadFiles :: [FilePath] -> IO [BL.ByteString]
loadFiles = sequence . fmap BL.readFile

-- | Load cached files.
loadCachedEntries :: MuseConf -> IO [BL.ByteString]
loadCachedEntries = lsEntryCache >=> loadFiles

decodeEntries :: [BL.ByteString] -> [Maybe [LogEntry]]
decodeEntries = fmap decode

decodeCachedEntries :: MuseConf -> IO [Maybe [LogEntry]]
decodeCachedEntries = fmap decodeEntries <$> loadCachedEntries

-- | Parse all entries from logDir into cacheDir/entries.
parseAllEntries :: Bool -> Bool -> MuseConf -> IO ()
parseAllEntries quiet ignoreCache mc@(MuseConf log cache home)
  -- read in log file names; parse 'em
  --putStrLn $ show mc
 = do
  fps <- sort <$> lsEntrySource mc
  -- 
  let selectModified :: [FilePath] -> IO [FilePath]
      -- | Check, if for a given log file a parsed file has been cached, 
      --   whether the log's modification date is greater than the that of the 
      --   cached json.
      selectModified fps =
        if ignoreCache
          then putStrLn "ignoring parsed entry cache" >> return fps
          else foldr
                 (\fp rest
                    -- check for cache existence
                   -> do
                    existsCache <-
                      doesFileExist $ T.unpack (entryCache mc) ++ fp
                    if existsCache
                      -- compare  and log modification times
                      then do
                        logMd <-
                          getModificationTime $ T.unpack (entrySource mc) ++ fp
                        cacheMd <-
                          getModificationTime $ T.unpack (entryCache mc) ++ fp
                        if cacheMd < logMd -- FIXME reparses today's log since filenames
                          then do
                            putStrLn $ "Update/add: " ++ fp
                            (fp :) <$> rest
                          else rest -- exclude unchanged
                      else (fp :) <$> rest)
                 (return [])
                 fps
      parse :: String -> IO (String, Either String [LogEntry])
      parse fp =
        (,) <$> pure fp <*>
        (showErr . Tri.parseByteString logEntries mempty <$>
         B.readFile (T.unpack (entrySource mc) ++ "/" ++ fp))
      invert :: (fp, Maybe e) -> Maybe (fp, e)
      invert (fp, me) =
        case me of
          Just e -> Just (fp, e)
          Nothing -> Nothing
      -- TODO group `logEntry`s by day for use with 'addDay'
      parseAndShowErrs :: [FilePath] -> IO [(String, [LogEntry])]
      parseAndShowErrs fs = sequence (parse <$> fs) >>= showOrCollect
      showOrCollect :: [(String, Either String res)] -> IO [(String, res)]
      showOrCollect =
        let sideBar = unlines . fmap ("> " ++) . lines
        in foldr
             (\(fp, e) rest ->
                case e -- render errros w filename, `ErrInfo`
                      of
                  Left err ->
                    if quiet
                      then rest
                      else putStrLn ("File: " ++ fp ++ "\n" ++ sideBar err) >>
                           rest
                  Right res -> do
                    if showDebug
                      then putStrLn $ "Success: " ++ fp
                      else return ()
                    ((fp, res) :) <$> rest)
             (return [])
  if quiet
    then putStrLn "\nSuppressing entry parse error output"
    else return ()
  entryGroups <- selectModified fps >>= parseAndShowErrs
  -- TODO add to DB here
  sequence_ $
    fmap
      (\(fp, eg) ->
         BL.writeFile (T.unpack (entryCache mc) ++ "/" ++ fp) (encode eg))
      entryGroups

-- TODO 
--
-- ▣  centralize file path generation--save yourself the headache later of
--    mismatched paths!
--
--    See `entryCache` and `entrySource`
-- on parse failure, show user err info
-- | Parse all entries from logDir into cacheDir/entries.
parseAllEntries' :: Bool -> Bool -> MuseConf -> IO ()
parseAllEntries' quiet ignoreCache mc@(MuseConf log cache home)
  -- read in log file names; parse 'em
  --putStrLn $ show mc
 = do
  fps <- sort <$> lsEntrySource mc
  -- 
  let selectModified :: [FilePath] -> IO [FilePath]
      -- | Check, if for a given log file a parsed file has been cached, 
      --   whether the log's modification date is greater than the that of the 
      --   cached json.
      selectModified fps =
        if ignoreCache
          then putStrLn "ignoring parsed entry cache" >> return fps
          else foldr
                 (\fp rest
                    -- check for cache existence
                   -> do
                    existsCache <-
                      doesFileExist $ T.unpack (entryCache mc) ++ fp
                    if existsCache
                      -- compare  and log modification times
                      then do
                        logMd <-
                          getModificationTime $ T.unpack (entrySource mc) ++ fp
                        cacheMd <-
                          getModificationTime $ T.unpack (entryCache mc) ++ fp
                        if cacheMd < logMd -- FIXME reparses today's log since filenames
                          then do
                            putStrLn $ "Update/add: " ++ fp
                            (fp :) <$> rest
                          else rest -- exclude unchanged
                      else (fp :) <$> rest)
                 (return [])
                 fps
      parse :: String -> IO (String, Either String [LogEntry])
      parse fp =
        (,) <$> pure fp <*>
        (showErr . Tri.parseByteString logEntries mempty <$>
         B.readFile (T.unpack (entrySource mc) ++ "/" ++ fp))
      invert :: (fp, Maybe e) -> Maybe (fp, e)
      invert (fp, me) =
        case me of
          Just e -> Just (fp, e)
          Nothing -> Nothing
      -- TODO group `logEntry`s by day for use with 'addDay'
      parseAndShowErrs :: [FilePath] -> IO [(String, [LogEntry])]
      parseAndShowErrs fs = sequence (parse <$> fs) >>= showOrCollect
      showOrCollect :: [(String, Either String res)] -> IO [(String, res)]
      showOrCollect =
        let sideBar = unlines . fmap ("> " ++) . lines
        in foldr
             (\(fp, e) rest ->
                case e -- render errros w filename, `ErrInfo`
                      of
                  Left err ->
                    if quiet
                      then rest
                      else putStrLn ("File: " ++ fp ++ "\n" ++ sideBar err) >>
                           rest
                  Right res -> do
                    if showDebug
                      then putStrLn $ "Success: " ++ fp
                      else return ()
                    ((fp, res) :) <$> rest)
             (return [])
  if quiet
    then putStrLn "\nSuppressing entry parse error output"
    else return ()
  entryGroups <- selectModified fps >>= parseAndShowErrs
  let --x :: _
      x =
        bracket
          (openLocalStateFrom "state/DB" initDB)
          createCheckpointAndClose
          (\acid -> do
             sequence $
               fmap (\(d, le) -> update acid $ AddDay d le) $
               catMaybes $
               fmap (\(a, b) -> (,) <$> pathToDay a <*> Just b) entryGroups)
  -- FIXME as yet writes to acid-state and file-system persistence solutions
  sequence_ $
    fmap
      (\(fp, eg) ->
         BL.writeFile (T.unpack (entryCache mc) ++ "/" ++ fp) (encode eg))
      entryGroups
