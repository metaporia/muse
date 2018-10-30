module Main where

import Lib
import Parse

import Options.Applicative
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import qualified Text.Trifecta.Result as Tri
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)

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
relDurReader = eitherReader $ \s -> 
  case parse relDur s of
    Tri.Success rd -> Right rd
    Tri.Failure err -> Left $ "Cannot parse relative date by (dmy): " ++ s ++ "\nErrInfo: " ++ show err

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
  , map    :: Entry -> a
  }

data DateTime = DateTime 

data SearchResult = Def' String
                  | Quotation' Quote Author
                  | Commentary' Body
                  | Read' Title Author
                  -- | Entry' String -- ?
                  deriving (Eq, Show)

-- | Represents filters and entry maps extracted from CLI invocation.
data Input a = Input
  { startDateTime :: Day
  , endDateTime :: Day
  , authorPred :: String -> Bool
  , titlePred :: String -> Bool
  , mapped :: Entry -> a
  }



-- | Convert duration, combined with the system time, into UTC time. See the
-- `time` library.
--
-- TODO: handle d/m/y excess w/ rollover
toUTC :: Day -> RelDur -> Day
toUTC day (RelDur y m d) =
  addGregorianYearsRollOver y . addGregorianMonthsRollOver m . addDays d $ day

search :: Day -> Parser (Input SearchResult)
search today = Input
      <$> (toUTC today <$> within)
      <*> pure today
      <*> fmap isInfixOf author 
      <*> fmap isInfixOf title
      <*> pure entryToSearchResult

entryToSearchResult :: Entry -> SearchResult
entryToSearchResult (Def dq) = Def' (show dq)
entryToSearchResult (Quotation b attr) = Quotation' b attr
entryToSearchResult (Read t a) = Read' t a
entryToSearchResult (Commentary s) = Commentary' s

author :: Parser String
author = strOption (long "author" 
      <> short 'a' 
      <> value ""
      <> help "Substring/affix of author")

title :: Parser String
title = strOption (long "title"
      <> short 't'
      <> value "" 
      <> help "Affix of title")

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int
  , rd         :: RelDur }


sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )

      -- <*> option auto
      --    ( long "
      --    )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

      <*> option relDurReader 
         ( long "within"
        <> help "How far back within logs to apply search filters"
        <> short 'w'
        <> value (RelDur 0 6 0)
         )

within :: Parser RelDur
within = option relDurReader 
         ( long "within"
        <> help "How far back within logs to apply search filters"
        <> short 'w'
        <> value (RelDur 0 6 0)
         )

main' :: IO ()
main' = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  let opts = info (search today <**> helper)
            ( fullDesc
           <> progDesc "Run logParse search filters."
           <> header "muse - a reading log search interface" )
  runSearch =<< execParser opts

runSearch :: Input SearchResult -> IO ()
runSearch (Input s e _ _ _) = putStrLn $ show s ++ "\n" ++ show e ++ "\nfancy search magick!"

greet :: Sample -> IO ()
greet (Sample h False n rd) =
  putStrLn $ "Hello, " ++ h ++ replicate n '!' ++ ('\n' : show rd)
greet _ = return ()
