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
import           Prelude hiding (lookup, log)
import           System.Directory (doesFileExist, createDirectoryIfMissing, listDirectory)
import           System.Environment (getEnv)
import           Text.Show.Pretty (pPrint)
import qualified Text.Trifecta.Result as Tri



-- | Represents filters and entry maps extracted from CLI invocation.
data Input a = Input
  { startDateTime :: Day
  , endDateTime :: Day
  , authorPred :: Author -> Bool
  , titlePred :: Title -> Bool
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
  | Null'
                  -- | Entry' String -- ?
  deriving (Eq, Show)


x :: Input SearchResult -> [LogEntry]
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
