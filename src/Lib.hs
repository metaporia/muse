{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module merely exports modules "Diff", "Helpers", "Parse", "Search",
-- and "Render".
--
-- TODO
--
-- ▣   PARSER BUG: '<ts> <unprefixed-word>'  silently fails and prevents parsing
--    of the rest of the log file.
--
-- ▣   SEARCH BUG: search ```muse -c search -s3y -d --dh obeisance```
--    fails to yield the "obeisance, obedience" entry at 15:49:25 on 19.03.02;
--    likewise for another plural definition entry containing "inure",
--    "immure", and "durance" on the same day.
--
-- ▣   use case insensitive search
--
-- ▣   fix phrase meaning rendering: newlines are not stripped, so what
--    indentation suits the log file's plain text origin may not suit the
--    CLI's prettified output.
--
-- ▣   (!!!) usablility: dispatch bucket filters so that a option for a given
--     flag, say, `--qb <word>`, implies the presence of its associated bucket type
--     filter, in this
--     case, `-q`.
--
-- □  (!!) fix change phrase search option to --ph; standing for "phrase head":
--    as yet the --phrase is both a flag and an option, the latter having been
--    shadowed by the former.
--
-- ▣  (!!!) PARSER: clean up (that is, mangle further) non-compliant log files
--    and/or update parser to accomodate desirable aspects of the old syntax.
--
--    DIRECTIVE: fail loudly: WE DO NOT WANT HALF-PARSED FILES ANY MORE
--
--
-- ▣  (convenience) CLI: add `search --all` option to include all entries in
--    search.
--
-- □  switch to list identifying characteristics of entries, viz. full
--    truncated UTCTime and entry variant
-- □  fetch entry by timestamp & variant
-- □  create absolute paths to entries by appending log file name and entry
--    timestamp; then store log of review times of entry ids, w type of entry;
--    this will ease efficient review of quotes.
-- □  replace --qb with --qt. We must forego a short option, but we may yet
--    have an intuitive one.

-----------------------------------------------------------------------------
module Lib
  ( main
  , testSearchConfig
  )
where

import           CLI.Parser.Custom              ( caretedPreds
                                                , parseTags
                                                , pathToDay
                                                , reldur
                                                , searchArgument
                                                )
import           CLI.Parser.Types
import           Control.Lens                   ( _Left
                                                , over
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.State
import           Data.Aeson              hiding ( Null )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( isInfixOf
                                                , sort
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import           Data.Time
import           Data.Time.Clock                ( utctDay )
import           Data.Yaml.Config               ( load
                                                , lookupDefault
                                                )
import           Database.Persist.Sqlite        ( runMigration
                                                , runSqlite
                                                )
import           Helpers
import           Options.Applicative
import qualified Parse
import           Parse.Helpers                  ( parsePretty )
import           Parse.Types                    ( DefQuery
                                                , DefQueryVariant(..)
                                                , LogEntry
                                                , RelDur(..)
                                                , allDefVariants
                                                )
import           Prelude                 hiding ( init
                                                , log
                                                , lookup
                                                )
import           Render
import           Search
import           Store                   hiding ( Author
                                                , Search
                                                , Title
                                                , defs
                                                , quotes
                                                )
import           Store.Render                   ( )
import qualified Store.Sqlite                  as Sql
import           Store.Sqlite                   ( DefSearch(..)
                                                , SearchConfig
                                                , StrSearch(..)
                                                , fetchLastRead
                                                )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , getModificationTime
                                                , listDirectory
                                                )
import           System.Environment             ( getEnv )
import           System.Exit                    ( exitFailure )
import           Text.Megaparsec                ( errorBundlePretty )
import qualified Text.Megaparsec               as M
import           Text.Show.Pretty               ( pPrint )

version :: String
version = "muse 0.3.0"

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
  eitherReader $ \s -> over _Left errorBundlePretty $ M.parse reldur "" s
  -- d m y
  -- y m d
  -- m d y

-- | Convert duration, combined with the system time, into UTC time. See the
-- `time` library.
subRelDur :: Day -> RelDur -> Day
subRelDur day (RelDur y m d) =
  addGregorianYearsRollOver (negate y)
    . addGregorianMonthsRollOver (negate m)
    . addDays (negate d)
    $ day

search :: Day -> Parser SearchConfig
search today = do

  authPreds  <- authorS
  titlePreds <- titleS

  attrs      <-
    fmap
        (<>)
        (   fmap
            (\s (Attribution _ a) ->
              (toLower <$> s) `isInfixOf` T.unpack (T.toLower a)
            )
        <$> authorS
        )
      <*> (   fmap
              (\s (Attribution t _) ->
                (toLower <$> s) `isInfixOf` T.unpack (T.toLower t)
              )
          <$> titleS
          )

  defSearch <- parseDefSearch

  dvs       <- flag
    []
    [DefVersus']
    (long "dvs" <> long "def-versus" <> help "Select definition comparisons.")

  ds <- switch $ long "definitions" <> short 'd' <> help "Collect definitions."
  tags  <- tagsList
  collectPhrases    <- switch $ long "phrases" <> short 'p' <> help "Collect phrases."
  qs    <- switch $ long "quotes" <> short 'q' <> help "Collect quotes."
  dials <-
    switch
    $  long "dialogues"
    <> long "dias"
    <> long "dia"
    <> long "dial"
    <> help "Collect dialogues."
  cmts <- switch $ long "comments" <> long "cmts" <> help "Collect comments."
  dmps      <- switch $ long "dumps" <> long "dmps" <> help "Collect dumps."

  s         <- subRelDur today <$> (sinceAll <|> since)

  dhw'      <- defHW
  dm'       <- defMeaning
  q'        <- quoteBody
  --phw'      <- phraseHW
  --pm'       <- phraseMeaning
  dias'     <- dialogueBody
  comments' <- commentBody

  pure $ Sql.SearchConfig
    -- since
    s
    -- before: since the time of today is truncated we search for entries occuring before tommorow to include today's logs.
    (addDays 1 today)
    dmps
    qs
    dials
    cmts
    (ds || collectPhrases)
                        -- turn search strings into infx
                        -- searches for SQL's LIKE clause
    (padForSqlLike <$> authPreds, padForSqlLike <$> titlePreds)
    ((\ds@DefSearch { defVariants } -> ds { defVariants = defVariants ++ dvs })
      defSearch
    )
    (padForSqlLike <$> q')
    (padForSqlLike <$> comments')
    (padForSqlLike <$> dias')
    []
    (if collectPhrases then "phrase":tags else tags)

padForSqlLike s = '%' : s ++ "%"

-- mkSearchConfig since before checkDumps checkQuotes checkDialogues checkComments checkDefs check

data InputType
  = File FilePath
  | StdIn String
  | All Bool Bool --  Parse all entries in `entrySource`
     --  Silence errors?
     --  Ignore parsed entry cache?
  deriving (Eq, Show)

-- | A bare invocation will default to `parse --all --ignore-cache`, which
-- parses all entries, uses parsed entry cache, and displays parse errors.
parse' :: Parser InputType
parse' = File <$> fileInput <|> StdIn <$> stdInput <|> allEntries <|> pure
  (All False False)

allEntries :: Parser InputType
allEntries =
  (\_ s i -> All s i)
    <$> switch
          (long "all" <> short 'a' <> help
            "Parse all entries in 'log-dir' (see ~/.muse/config.yaml)"
          )
    <*> quiet
    <*> ignore

stdInput :: Parser String
stdInput =
  option
      str
      (metavar "LOG_CONTENT" <> long "stdin" <> short 's' <> help
        "Parse from stdin"
      )
    <**> helper

fileInput :: Parser FilePath
fileInput =
  option
      str
      (long "file" <> metavar "FILE" <> help "Read in log from file" <> short
        'f'
      )
    <**> helper

quiet :: Parser Bool
quiet = switch
  (long "quiet" <> short 'q' <> help "Suppress log parser error messages")

ignore :: Parser Bool
ignore = switch
  (long "ignore-cache" <> short 'i' <> help
    "Reparse all entries, ignore cache (overwrites cache)"
  )

color :: Parser Bool
color =
  switch (long "color" <> short 'c' <> help "Colorize output; off by default")
  --(infoOption "muse 0.1.5" $
  -- long "version" <> short 'V' <> help "Display version information") <*>

toplevel :: Day -> Parser Opts
toplevel today
  = (Opts <$> color <*> subparser
      (  command
          "search"
          (info
            (Search <$> search today <**> helper)
            (progDesc
              "Search log entries by date, author, title, or predicate on entry contents.\
             \ Inline definitions of headwords or phrases can be searched as well."
            )
          )
      <> command
           "parse"
           (info
             (Parse <$> parse' <**> helper)
             (progDesc "Parse entries; a bare invocation runs Rparse --allR")
           )
      <> command
           "lastRead"
           (info
             (    FetchLastRead
             <$>  switch
                    (  long "suppress-newline"
                    <> help "Suppress trailing newline"
                    )
             <**> helper
             )
             (progDesc "Fetches most recent \"read\" entry.")
           )
      <> command
           "lint"
           (info (pure Lint)
                 (progDesc "TBD; for, e.g., author attribution validation")
           )
      <> command
           "init"
           (info
             ((Init <$> quiet <*> ignore) <**> helper)
             (progDesc
               "Initialize config file, cache directory, and entry log\
             \ directory; parse all entries in Rlog-dirR"
             )
           )
      )
    )
    <|> (infoOption version (long "version" <> short 'V') <*> pure Bare) -- VERSION



data SubCommand
  = Search Sql.SearchConfig
  | Parse InputType
  | FetchLastRead Bool -- toggles trailing newline suppression
  | Lint
  | Init Bool
          Bool
  deriving Show

data Opts
  = Opts { colorize :: Bool
          , subcommand :: SubCommand }
  | Bare
  deriving Show

-- | Definition parser in new style.
--
--  * Select definition entries with @-d@/@--def@/@--defs@/@--definition@/@--definitions@.
--  * Apply (infix, a.t.m.) string searches on both headword and meaning with
--  @--ds@/@--def-search@.
--
-- The definition search option takes one string argument (@<string-arg>@)
-- which it consumes according to the below grammar.
--
--
-- Search argument grammar:
--
--    Note that around the colon whitespace is discarded but within a search
--    string list is kept and included in the search.
--
--    <search-arg>              ::= <headword-meaning-search>
--                                | <headword-search>
--                                | <meaning-search>
--    <headword-meaning-search> ::= <predicates> ":" <predicates>
--    <headword-search>         ::= <predicates> | <predicates> ":"
--    <meaning-search>          :: = ":" <predicates>
--    <predicates>              ::= <word> { <sep> <word> }
--    <sep>                     ::= "&" | "|"
--    <word>                    ::= <char>+
--    <char>                    ::= [a-zA-Z]
--
-- Applying only one search string to headword and/or meaning:
--
--    * @rep@ or @rep :@ : select entries whose headworsd contain "rep"
--    * @:censure@ : select entries (with inline definitions) whose meanings
--    contains "censure"
--    * @rep:censure@ : selects entries that satisfy both of the above
--    constraints
--
-- Searches with with conjunction/disjunction:
--
--    * @reprieve|respite@ : select entries whose headword matches either
--    * @re&oof@ : select entries whose headword matches both
--    * @re&oof:latter|delay@ : select entries whose headword satisfy the
--    above and whose meaning contains either "latter" or "delay"
--

-- For the time being, the @-d@ option will select all matching definitions of
-- all variants to which the extracted predicates are applicable. That is, if
-- only @-d resp@ is received, then /all/ definition types that satisfy will be
-- included in the output. However, if the search argument is instead of the
-- form @-d :meaning@, only satisfactory comparisons and inline definitions
-- will be included in the output as we cannot reasonably apply a meaning
-- search predicate to definition entries which contain only a headword.
--
-- We could supply @--dvs@, @--inline@, @--phrase@ options that each specify a
-- different 'DefQueryVariant'.
--
-- REFACTOR NOTE: Proprosed replacement as part of search string padding deferral.
parseDefSearch :: Parser DefSearch
parseDefSearch =
  (   (\case
        (Just hw, Just mn) ->
          DefSearch [InlineDef', DefVersus'] (Just hw) (Just mn)
        (Nothing, Just mn) -> DefSearch [InlineDef', DefVersus'] Nothing (Just mn)
        (Just hw, Nothing) -> DefSearch allDefVariants (Just hw) Nothing
        -- this is really an error, right? yes. see 'searchArgument' it's
        -- impossible
        (Nothing, Nothing) -> error "searchArgument type signature violated" --DefSearch [] [] []
      )
    <$> defSearchFlag
    )
    <|> pure (DefSearch [] Nothing Nothing)


-- search string deferral revision
defSearchFlag
  :: Parser
       ( Maybe (BoolExpr (StrSearch String))
       , Maybe (BoolExpr (StrSearch String))
       )
defSearchFlag = option
  (eitherReader (parsePretty searchArgument))
  (  long "def-search"
  <> long "ds"
  <> help
       "Consume \"<headword-preds> : <meaning-preds>, see README for search syntax."
  )



-- variant flag
-- beginning of rewrite
defHW :: Parser [String]
defHW =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "def-headword"
    <> long "dh"
    <> value []
    <> help "Collect defs that satisfy headword search."

defMeaning :: Parser [String]
defMeaning =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "dm"
    <> long "def-meaning"
    <> value []
    <> help "Search for strings within meaning/definition."

-- variant flag
phraseHW :: Parser [String]
phraseHW =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  (long "phr-hw" <> long "ph" <> value [])
    <> help "Collect only satisfactory phrases"

phraseMeaning :: Parser [String]
phraseMeaning =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  (long "pm" <> long "phr-meaning" <> value [])
    <> help "Search for strings within meaning/definition."

-- | Restrict search to entries that exactly match all tags passed via the
-- `--tags/--tag/-T` option. The string argument is expected to be a comma separated
-- list of tags. A tag must match the regex @[a-zA-Z0-9_-]+@.
tagsList :: Parser [TL.Text]
tagsList =
  option tags $ long "tags" <> long "tag" <> short 'T' <> value [] <> help
    "Restrict search to entries with the given comma-separted tags."
  where tags = eitherReader $ parsePretty parseTags . TL.pack

-- variant flag
quoteBody :: Parser [String]
quoteBody =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "quote-body"
    <> long "qb"
    <> value ""
    <> help "Collect satisfactory quotes."

-- variant flag
dialogueBody :: Parser [String]
dialogueBody =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "dialogue-body"
    <> long "db"
    <> value []
    <> help "Collect satisfactory dialogues."

-- variant flag
commentBody :: Parser [String]
commentBody =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "comment-body"
    <> long "cb"
    <> value []
    <> help "Collect satisfactory comments."

-- end of rewrite
authorS :: Parser [String]
authorS =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "author"
    <> short 'a'
    <> value []
    <> help "Collect entries attributed to satisfactory authors."

-- end of rewrite
titleS :: Parser [String]
titleS =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  long "title"
    <> short 't'
    <> value []
    <> help "Collect entries attributed to satisfactory authors."

parsePreds :: String -> Either String [String]
parsePreds = parsePretty caretedPreds

-- | FIXME: cache date of first log file.
sinceAll :: Parser RelDur
sinceAll = flag' (RelDur 10 0 0) (short 'a' <> long "all")

since :: Parser RelDur
since = option
  relDurReader
  (  long "since"
  <> metavar "REL_DATE"
  <> help "Lower bound of search filter range"
  <> short 's'
  <> value (RelDur 0 6 0)
  )

showDebug = False

-- | This is currently in use.
main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  execParser (info (helper <*> toplevel today) (fullDesc <> header "dispatch"))
    >>= dispatch

-- | (For development purposes only.) Test CLI parser from ghci.
testMain :: String -> IO ()
testMain s = do
  today <- utctDay <$> getCurrentTime
  let result =
        execParserPure
            defaultPrefs
            (info (helper <*> toplevel today) (fullDesc <> header "dispatch"))
          $ words s
  case result of
    Options.Applicative.Success opts -> dispatch opts
    _ -> pPrint result

testSearchConfig :: String -> IO (Maybe SearchConfig)
testSearchConfig s = do
  today <- utctDay <$> getCurrentTime
  let res =
        execParserPure defaultPrefs (info (search today) briefDesc) $ words s
  return $ case res of
    Options.Applicative.Success opts -> Just opts
    _ -> Nothing

-- TODO
--
-- □  lint: run parse but don't write to DB
--
dispatch :: Opts -> IO ()
dispatch Bare = putStrLn version
dispatch opts@(Opts color (Search searchConfig)) = do
  mc <- loadMuseConf
  runSqlite (sqlDbPath mc) $ do
    runMigration Sql.migrateAll
    results <- Sql.dispatchSearch (T.unpack $ home mc <> "/logs") searchConfig -- FIXME decide as to loging/log path
    -- FIXME only show successes
    liftIO $ pPrint searchConfig
    liftIO $ showAll results
dispatch (Opts color Lint                    ) = putStrLn "linting"
dispatch (Opts color (Init quiet ignoreCache)) = do
  putStrLn "initializing...\n" -- ++ showMuseConf mc
  void $ museInit quiet ignoreCache
dispatch opts@(Opts color (FetchLastRead suppressNewline)) = do
  mc <- loadMuseConf
  runSqlite (sqlDbPath mc) $ do
    -- FIXME searches on
    lastRead <- fetchLastRead
    let put = if suppressNewline then putStr else putStrLn -- default
    liftIO $ case lastRead of
      Just (t, a) -> put $ "read \"" <> t <> "\" by " <> a
      Nothing     -> exitFailure
dispatch (Opts color (Parse it)) = do
  mc <- loadMuseConf
  putStrLn "parsing..."
  s <- case it of
    File  fp           -> readFile fp
    StdIn s            -> return s
    All silence ignore -> parseAllEntries silence ignore mc >> return ""
  putStrLn s


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

sqlDbPath :: MuseConf -> T.Text
sqlDbPath (MuseConf _ _ home) = home <> "/.muse/state/sqlite.db"

config :: MuseConf -> T.Text
config mc = home mc <> "/.muse/config.yaml"

-- | Expects config at  $HOME/.muse/config.yaml
loadMuseConf :: IO MuseConf
loadMuseConf = do
  home'  <- getEnv "HOME"
  config <- load $ home' ++ "/.muse/config.yaml"
  let home     = T.pack home'
      logDef   = home <> "/.muse/entries/"
      cacheDef = home `T.append` "/.cache/muse/"
      -- lookup
      logDir   = lookupDefault "log-dir" logDef config
      cacheDir = lookupDefault "cache-dir" cacheDef config
  --  mapM_ T.putStrLn ["muse config: ", logDir, cacheDir]
  return $ MuseConf logDir cacheDir home

writeMuseConf :: MuseConf -> IO MuseConf
writeMuseConf mc = do
  let conf =
        "log-dir: " <> entrySource mc <> "\n\ncache-dir: " <> entryCache mc
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
  let home        = T.pack home'
      defConfPath = home <> "/.muse"
      defCacheDir = home <> "/.cache/muse"
  T.putStr
    $  "Enter path to entry directory (default: "
    <> defConfPath
    <> "/entries/): "
  resp <- T.getLine
  let entryDir :: T.Text
      entryDir = case resp of
        "" -> defConfPath <> "/entries"
        _  -> resp
  T.putStr $ "Enter path to cache directory (default: " <> defCacheDir <> "): "
  resp <- T.getLine
  let cacheDir = case resp of
        "" -> defCacheDir
        _  -> resp
      conf = MuseConf entryDir defConfPath cacheDir
  writeMuseConf conf

-- | Creates ~/.muse/{entries/,config.yaml} and ~/.cache/muse/entries.
--
--
--  Creates default config directory, "$HOME/.muse/", only if it does not
--  already exist.
museInit :: Bool -> Bool -> IO MuseConf
museInit quiet ignoreCache = do
  home' <- getEnv "HOME"
  -- TODO consistently prepend forwardslashes in 'MuseConf' paths
  let home              = T.pack home'
      defaultConfigPath = home <> "/.muse/config.yaml"
      defaultCacheDir   = home <> "/.cache/muse/"
      defaultLogDir     = home <> "/.muse/entries/"
      defaultMuseConf   = MuseConf defaultLogDir defaultCacheDir home
  T.putStrLn $ "Expects configuration file at: " <> defaultConfigPath <> "\n"
  doesExist <- doesFileExist $ T.unpack defaultConfigPath
  museConf  <- if doesExist then loadMuseConf else writeMuseConf defaultMuseConf
  createDirectoryIfMissing True . T.unpack $ entryCache museConf
  T.putStrLn
    $  entryCache museConf
    <> " and "
    <> log museConf
    <> " found or created."
  createDirectoryIfMissing True $ T.unpack (log museConf)
  parseAllEntries quiet ignoreCache museConf
  return museConf



-- | List file names in log source directory
lsEntrySource :: MuseConf -> IO [FilePath]
lsEntrySource = listDirectory . T.unpack . entrySource

-- | Parse all entries from logDir into cacheDir/entries.
-- TODO
--
-- ▣  centralize file path generation--save yourself the headache later of
--    mismatched paths!
--
--    See `entryCache` and `entrySource`
-- on parse failure, show user err info
-- | Parse all entries from logDir into cacheDir/entries.
parseAllEntries :: Bool -> Bool -> MuseConf -> IO ()
parseAllEntries quiet ignoreCache mc@(MuseConf log cache home) = do
  fps <- sort <$> lsEntrySource mc
  let
    -- If we always cache (modified) parsed LogEntry groups then we need only
    -- store last parse time. Then we select all logs modified since the last
    -- parse and pass those into 'parseAndShowErrs' unless @ignoreCache ==
    -- True@.
    --
    -- TODO Test that parse invocation with @ignoreCache == True@ i) reparses
    -- all logs and ii) replaces the entire cache.
    selectModified :: [FilePath] -> UTCTime -> IO [FilePath]
    selectModified fps lastParsed =
      let hasBeenModified :: FilePath -> IO Bool
          hasBeenModified baseName = do
            dateModified <-
              getModificationTime $ T.unpack (entrySource mc) ++ baseName
            return (dateModified > lastParsed)
      in  filterM
            (\fp ->
              (&&) <$> hasBeenModified fp <*> return (isJust (pathToDay fp))
            )
            fps
    --parse :: String -> IO (String, Either (ErrorBundle ...) [LogEntry])
    parse fp = do
      eitherLogEntry <- Parse.parseLogEntries
        <$> T.readFile (T.unpack (entrySource mc) <> "/" <> fp)
      return (fp, fst eitherLogEntry)
    parseAndShowErrs :: [FilePath] -> IO [(String, [LogEntry])]
    parseAndShowErrs fs = sequence (parse <$> fs) >>= showOrCollect
    showOrCollect
      = let sideBar = unlines . fmap ("> " ++) . lines
        in
          foldr
            (\(fp, e) rest -> case e of
              Left err -> if quiet
                then rest
                else
                  putStrLn
                      ("File: " ++ fp ++ "\n" ++ sideBar (errorBundlePretty err)
                      )
                    >> rest
              Right res -> do
                when showDebug $ putStrLn $ "Success: " ++ fp
                ((fp, res) :) <$> rest -- render errros w filename, `ErrInfo`
            )
            (return [])
  when quiet $ putStrLn "\nSuppressing entry parse error output"
  runSqlite (home <> "/.muse/state/sqlite.db") $ do
    -- FIXME remove before merging feature into master.
    -- Migration should be performed manually on both test and "production"
    -- databases
    runMigration Sql.migrateAll
    now          <- liftIO getCurrentTime
    entriesByDay <- if ignoreCache
      then liftIO $ parseAndShowErrs $ filter (isJust . pathToDay) fps
      else do
        lastParseTime <- fromMaybe now <$> Sql.getLastParseTime
        liftIO $ selectModified fps lastParseTime >>= parseAndShowErrs
    -- write to sqlite db
    traverse_ (uncurry Sql.writeDay)
      $ mapMaybe (\(a, b) -> (,) <$> pathToDay a <*> Just b) entriesByDay
    -- /Always/ update cache: when ignoreCache is true, this will
    -- overwrite the lot; but when false, will overwrite only the modified.
    --
    -- That is, ignoreCache only indicates whether to use the cache to speed up
    -- parsing, not that we should omit to /update/ the cache.
    liftIO $ traverse_
      (\(fp, logs) ->
        BL.writeFile (T.unpack (entryCache mc) ++ "/" ++ fp) (encode logs)
      )
      entriesByDay
    -- update LastParse
    -- FIXME LastParse only reflects the time of the last successful parses.
    Sql.setLastParseTime now ignoreCache
  return ()
