{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, ApplicativeDo, RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
-- This module merely exports modules "Diff", "Helpers", "Parse", "Parse.Entry", 
-- "Search", and "Render".
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
-- □   (!!!) usablility: dispatch bucket filters so that a option for a given
--     flag, say, `--qb <word>`, implies the presence of its associated bucket type 
--     filter, in this
--     case, `-q`.
--
-- □  (!!) fix change phrase search option to --ph; standing for "phrase head":
--    as yet the --phrase is both a flag and an option, the latter having been
--    shadowed by the former.
--
-- □  (!!!) PARSER: clean up (that is, mangle further) non-compliant log files 
--    and/or update parser to accomodate desirable aspects of the old syntax.
--
--    DIRECTIVE: fail loudly: WE DO NOT WANT HALF-PARSED FILES ANY MORE
--
--
-- □  (convenience) CLI: add `search --all` option to include all entries in
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
  )
where

import           Control.Exception              ( bracket )
import           Control.Monad                  ( (>=>)
                                                , join
                                                , void
                                                )
import           Control.Monad.State

import           Data.Aeson              hiding ( Null )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                )
import qualified Data.Monoid                   as M
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( fold , traverse_)
import           Data.List                      ( isInfixOf , sort)
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , mapMaybe
                                                , maybe
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time
import           Data.Time.Clock                ( utctDay )
import           Data.Yaml.Config               ( load
                                                , lookupDefault
                                                )
import           Database.Persist.Sqlite        ( runMigration
                                                , runSqlite
                                                )
import           Debug.Trace                    ( trace )
import           Helpers
import           Options.Applicative
import           Parse
import           Parse.Entry
import           CLI.Parser.Custom
import           CLI.Parser.Types
import           Prelude                 hiding ( init
                                                , log
                                                , lookup
                                                )
import           Render
import           Search
import           Store                   hiding ( Author
                                                , Search
                                                , Search'
                                                , Title
                                                , defs
                                                , quotes
                                                )
import           Store.Render ()
import qualified Store.Sqlite                  as Sql
import           Store.Sqlite                   ( SearchConfig )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , getModificationTime
                                                , listDirectory
                                                )
import           System.Environment             ( getEnv )
import           System.Exit                    ( exitFailure)
import           Text.Show.Pretty               ( pPrint )
import qualified Text.Trifecta                 as Tri
import qualified Text.Trifecta.Result          as Tri
import           Store.Sqlite                   ( StrSearch(..)
                                                , DefSearchR(..)
                                                , fetchLastRead
                                                )
version :: String
version = "muse 0.2.1"

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
relDurReader = eitherReader $ \s -> case parse reldur s of
  Tri.Success rd -> Right rd
  Tri.Failure err ->
    Left
      $  "Cannot parse relative date by (dmy): "
      ++ s
      ++ "\nErrInfo: "
      ++ show err
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

searchR :: Day -> Parser SearchConfig
searchR today = do

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

  -- defSearch <- parseDefSearch
  defSearchR <- parseDefSearchR

  dvs <- flag [] [DefVersus'] (long "dvs" <> long "def-versus" <> help "Select definition comparisons.")

  ds <- switch $ long "definitions" <> short 'd' <> help "Collect definitions."
  ps        <- switch $ long "phrases" <> short 'p' <> help "Collect phrases."
  qs        <- switch $ long "quotes" <> short 'q' <> help "Collect quotes."
  dials     <-
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
  phw'      <- phraseHW
  pm'       <- phraseMeaning
  dias'     <- dialogueBody
  comments' <- commentBody

  pure $ Sql.SearchConfig s
                        today
                        dmps
                        qs
                        dials
                        cmts
                        ds
                        -- turn search strings into infx
                        -- searches for SQL's LIKE clause
                        (padForSqlLike <$> authPreds, padForSqlLike <$> titlePreds)
                        ((\ds@DefSearchR { defVariantsR } -> ds { defVariantsR = defVariantsR ++ dvs} ) defSearchR)
                        (padForSqlLike <$> q')
                        (padForSqlLike <$> comments')
                        (padForSqlLike <$> dias')
                        []

padForSqlLike s = '%':s++"%"

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

toplevelR :: Day -> Parser OptsR
toplevelR today = (OptsR <$> color <*> subparser
  (  command
      "search"
      (info
        (SearchR <$> searchR today <**> helper)
        (progDesc
          "Search log entries by date, author, title, or predicate on entry contents.\
             \ Inline definitions of headwords or phrases can be searched as well."
        )
      )
  <> command
       "parse"
       (info
         (ParseR <$> parse' <**> helper)
         (progDesc "Parse entries; a bare invocation runs Rparse --allR")
       )
  <> command
       "lastRead"
       (info
         (    FetchLastReadR
         <$>  switch
                (long "suppress-newline" <> help "Suppress trailing newline")
         <**> helper
         )
         (progDesc "Fetches most recent \"read\" entry.")
       )
  <> command
       "lint"
       (info (pure LintR)
             (progDesc "TBD; for, e.g., author attribution validation")
       )
  <> command
       "init"
       (info
         ((InitR <$> quiet <*> ignore) <**> helper)
         (progDesc
           "Initialize config file, cache directory, and entry log\
             \ directory; parse all entries in Rlog-dirR"
         )
       )
  )) <|> (infoOption version (long "version" <> short 'V') <*> pure BareR) -- VERSION



data SubCommandR
  = SearchR Sql.SearchConfig
  | ParseR InputType
  | FetchLastReadR Bool -- toggles trailing newline suppression
  | LintR
  | InitR Bool
          Bool
  deriving Show

data OptsR
  = OptsR { colorizeR :: Bool
          , subcommandR :: SubCommandR }
  | BareR
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
parseDefSearchR :: Parser DefSearchR
parseDefSearchR = 
  ((\case
    (Just hw, Just mn) -> DefSearchR [InlineDef', DefVersus'] (Just hw) (Just mn)
    (Nothing, Just mn) -> DefSearchR [InlineDef', DefVersus'] Nothing (Just mn)
    (Just hw, Nothing) -> DefSearchR  allDefVariants (Just hw) Nothing
    -- this is really an error, right? yes. see 'searchArgument' it's
    -- impossible
    (Nothing, Nothing) -> error "searchArgument type signature violated" --DefSearch [] [] []
  ) <$> defRR) <|> pure (DefSearchR [] Nothing Nothing)


-- search string deferral revision
defRR
  :: Parser
       ( Maybe (BoolExpr (StrSearch String))
       , Maybe (BoolExpr (StrSearch String))
       )
defRR = option
  (eitherReader (parseEither searchArgumentRR))
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
    $  (long "phrases" <> short 'p' <> value [])
    <> help "Collect only satisfactory phrases"

phraseMeaning :: Parser [String]
phraseMeaning =
  fmap (either (const []) id . parsePreds)
    $  option str
    $  (long "pm" <> long "phr-meaning" <> value [])
    <> help "Search for strings within meaning/definition."

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

-- | Parse caret separated list of strings.
parsePreds :: String -> Either String [String] -- search type, rest of string
parsePreds s = case parse preds s of
  Tri.Success rd -> Right rd
  Tri.Failure err ->
    Left
      $  "Cannot parse '^' separated predicate list: "
      ++ s
      ++ "\nErrInfo: "
      ++ show err

-- | Run a "Trifecta" parser on a string and convert it to 'Either'.
parseEither :: Tri.Parser a -> String -> Either String a
parseEither p s = case parse p s of
  Tri.Success a   -> Right a
  Tri.Failure err -> Left $ "parseEither: error: " ++ show err

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
  execParser (info (helper <*> toplevelR today) (fullDesc <> header "dispatch"))
    >>= dispatchR

-- | (For development purposes only.) Test CLI parser from ghci.
testMain :: String -> IO ()
testMain s = do
  today <- utctDay <$> getCurrentTime
  let result =
        execParserPure
            defaultPrefs
            (info (helper <*> toplevelR today) (fullDesc <> header "dispatch"))
          $ words s
  case result of
    Options.Applicative.Success optsR -> dispatchR optsR
    _ -> pPrint result


-- TODO
--
-- □  (!) complete 'parseAllEntriesR' (caching (add table w date of insertion
-- for each entry, proper db path unification, etc.)
--
-- □  wrap 'dispatchSearch' to pretty print output; see 'colRender'
--
-- □  lint: run parse but don't write to DB
-- 
--
dispatchR :: OptsR -> IO ()
dispatchR BareR                           = putStrLn version
dispatchR opts@(OptsR color (SearchR searchConfig)) = do
  mc <- loadMuseConf
  runSqlite (sqlDbPath mc) $ do
    runMigration Sql.migrateAll
    results <- Sql.dispatchSearch (T.unpack $ home mc <> "/logs") searchConfig -- FIXME decide as to loging/log path
    -- FIXME only show successes
    liftIO $ pPrint searchConfig
    liftIO $ showAll results
    return ()
  return ()

    -- FIXME 
    -- bracket
    --(openLocalStateFrom (T.unpack (home mc) <> "/.muse/state/DB") initDB)
    --createCheckpointAndClose
    --(\acid -> do
    --  db <- query acid ViewDB
    --  putStrLn "searching...\n"
    --  runSearch' showDebug color s db
    --  return ()
    --)

dispatchR (OptsR color LintR                    ) = putStrLn "linting"
dispatchR (OptsR color (InitR quiet ignoreCache)) = do
  putStrLn "initializing...\n" -- ++ showMuseConf mc
  void $ museInitR quiet ignoreCache
dispatchR opts@(OptsR color (FetchLastReadR suppressNewline)) = do
  mc <- loadMuseConf
  runSqlite (sqlDbPath mc) $ do
    -- FIXME searches on
    lastRead <- fetchLastRead
    let put = if suppressNewline then putStr else putStrLn -- default
    liftIO $ case lastRead of
      Just (t, a) -> put $ "read \"" <> t <> "\" by " <> a
      Nothing     -> exitFailure
dispatchR (OptsR color (ParseR it)) = do
  mc <- loadMuseConf
  putStrLn "parsing..."
  s <- case it of
    File  fp           -> readFile fp
    StdIn s            -> return s
    -- TODO update 'parseAllEntries'
    All silence ignore -> parseAllEntriesR silence ignore mc >> return ""
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
museInitR :: Bool -> Bool -> IO MuseConf
museInitR quiet ignoreCache = do
  home' <- getEnv "HOME"
  -- TODO consistently prepend forwardslashes in 'MuseConf' paths
  let home = T.pack home'
      defaultConfigPath = home <> "/.muse/config.yaml"
      defaultCacheDir = home <> "/.cache/muse/"
      defaultLogDir = home <> "/.muse/entries/"
      defaultMuseConf = MuseConf defaultLogDir defaultCacheDir home
  T.putStrLn $ "Expects configuration file at: " <> defaultConfigPath <> "\n"
  doesExist <- doesFileExist $ T.unpack defaultConfigPath
  museConf <- if doesExist then loadMuseConf else writeMuseConf defaultMuseConf
  createDirectoryIfMissing True . T.unpack $ entryCache museConf
  T.putStrLn $ entryCache museConf <> " and " <> log museConf <> " found or created."
  createDirectoryIfMissing True $ T.unpack (log museConf)
  parseAllEntriesR quiet ignoreCache museConf
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


-- | Parse all entries into sqlite database.
--
-- TODO 
--
-- □  caching
--
-- □  overwrite vs insert 'wrteDay' mode
--
parseAllEntriesR :: Bool -> Bool -> MuseConf -> IO ()
parseAllEntriesR quiet ignoreCache mc@(MuseConf log cache home) = do
  fps <- sort <$> lsEntrySource mc
  -- 
      --  TODO rewrite this for sqlite
      -- 
      --
      -- 
      -- Check, if for a given log file a parsed file has been cached, 
      -- whether the log's modification date is greater than the that of the 
      -- cached json.
      --   
      --   1. if using cache, collect keys of modified entries from 'lastUpdated', otherwise use all
      --      * for each log source, fetch last modification time. if there is
      --        none, then add to list, otherwise compare the times: include only
      --        if the source file's modification date is more recent than that
      --        of 'lastUpdated`.
      --   2. convert to list
  let
    selectModified' fps lastUpdated = undefined -- TODO
      --let lastModified :: Day -> Maybe ModRec
      --    lastModified day = getOne $ lastUpdated @= (day :: Day)
      --    compare :: FilePath -> ModRec -> IO (Maybe FilePath)
      --    compare fp ModRec {..} = do
      --      logMd <- getModificationTime $ T.unpack (entrySource mc) ++ fp
      --      if logMd > modified
      --        then return $ Just fp
      --        else return Nothing
      --    f :: FilePath -> IO (Maybe FilePath)
      --    f fp =
      --      case pathToDay fp >>= lastModified of
      --        Just mr -> compare fp mr
      --        Nothing -> return $ Just fp
      --in foldr
      --     (\a bs -> f a >>= maybe bs (\x -> fmap (x :) bs))
      --     (return [] :: IO [FilePath])
      --     fps
      --filtermap pathToDay fps
    -- TODO (!!!!!) exclude days with duplicate primary keys!
    parse :: String -> IO (String, Either String [LogEntry])
    parse fp =
      (,)
        <$> pure fp
        <*> (showErr . Tri.parseByteString logEntries mempty <$> B.readFile
              (T.unpack (entrySource mc) ++ "/" ++ fp)
            )
    -- TODO group `logEntry`s by day for use with 'addDay'
    parseAndShowErrs :: [FilePath] -> IO [(String, [LogEntry])]
    parseAndShowErrs fs = sequence (parse <$> fs) >>= showOrCollect
    showOrCollect :: [(String, Either String res)] -> IO [(String, res)]
    showOrCollect =
      let sideBar = unlines . fmap ("> " ++) . lines
      in
        foldr
          (\(fp, e) rest -> case e of
            Left err -> if quiet
              then rest
              else putStrLn ("File: " ++ fp ++ "\n" ++ sideBar err) >> rest
            Right res -> do
              when showDebug $ putStrLn $ "Success: " ++ fp
              ((fp, res) :) <$> rest -- render errros w filename, `ErrInfo`
          )
          (return [])
  when quiet $ putStrLn "\nSuppressing entry parse error output"
  runSqlite (home <> "/.muse/state/sqlite.db") $ do
    runMigration Sql.migrateAll
    let allFiles = return fps
    entriesByDay <- liftIO $ allFiles >>= parseAndShowErrs
    traverse_ (uncurry Sql.writeDay)
      $ mapMaybe (\(a, b) -> (,) <$> pathToDay a <*> Just b) entriesByDay
  return ()
  -- read in log file names; parse 'em
  --putStrLn $ show mc


