{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Parse where

import Prelude hiding (any, lookup, min)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Text.Trifecta hiding (newline, rendered, render, Rendering, Span)
import Control.Applicative
import Data.Monoid ((<>))
import Data.Bifunctor
import Data.List (stripPrefix, isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Text.Trifecta.Result (Result(..))
import qualified Text.Trifecta.Result as Tri
import Data.Maybe (catMaybes)

import Control.Monad.Except
import Control.Monad.State.Class
import Lib

import Data.Maybe (maybe, isJust)

import Text.Show.Pretty

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure a) = Nothing




-- NB:  See ~/hs-note/src/Parse.hs for trifecta examples.

-- TODO write parsers for the following types from each pattern
-- □  from [r|hh:mm:ss λ.|] to TimeStamp
-- □  from, e.g., [| read "Title", by Author Name\n|] to (Title, Author)
-- □  from [r| d <def1>[, <def2>, ..., <defN>\n|] to [Definition]
--    where a <def> is "<word> [: <meaning>]" (the brackets '[' and ']'
--    indicate that the meaning, mapping is optional. The headword, <word>, is
--    not.
-- □  from
-- [r| dvs <def1>
--         --- vs ---
--         <def2> |] to  [DefVs]
-- □  from "q<pgNum> " to QuotationLocation
-- □



-- Datatypes

periodSep :: Parser String -> Parser [String]
periodSep p = p `sepBy` (symbol ".")

--commaSep :: Parser String -> Parser [String]
--commaSep p = p `sepBy` (symbol ",")

skipOptColon :: Parser ()
skipOptColon = skipOptional (char ':')

skipOptDot :: Parser ()
skipOptDot = skipOptional (char '.')

twoDigit :: Parser Int
twoDigit = read <$> count 2 digit <* skipOptColon

--timestamp :: Parser (Maybe TimeStamp)
--timestamp = fromList <$> read <$> periodSep twoDigit



timestamp :: Parser TimeStamp
--timestamp = fromList <$> fmap read <$> periodSep twoDigit
timestamp = TimeStamp <$> twoDigit 
                      <*> twoDigit 
                      <*> twoDigit  
                      <* space 
                      <* char 'λ' 
                      <* char '.'
                      <* space

-- | Definition parsing. The following are valid definition query forms:
--
--   * a comma separated query list
--
--     > "d word1[, ..., ]"
--
--      - works over multiple lines
--      - 
--
--   * for inline definition of headword 
--
--     > "d headword : meaning" 
--
--   * headword comparison
--
--     > "dvs headword1 : meaning
--     >      --- vs ---
--     >      headword2 : meaning"
defMultWords :: Parser [Def]
defMultWords = undefined --defQueryPrefix *> 


splitByComma = T.split (\c -> c == ',') 

newline :: Parser T.Text
newline = textSymbol "\n"

defQueryPrefix :: T.Text -> T.Text
defQueryPrefix t = case T.stripPrefix "d " t of
                     Just t -> t
                     Nothing -> error  "Expected defQueryPrefix \"d \""

dropQueryPrefix :: String -> String
dropQueryPrefix t = case stripPrefix "d " t of
                     Just t -> t
                     Nothing -> error $ "Expected defQueryPrefix \"d \"; found " ++ "\"" ++ take 4 t ++ "\""

pruneQuery' :: T.Text -> [T.Text]
pruneQuery' = splitByComma . defQueryPrefix 

data DefQuery = Plural [Headword]
              | Single Headword
              | InlineDef Headword Meaning
              | DefVersus Headword Meaning Headword Meaning
              deriving (Eq, Show)

testStrPlural :: String
testStrPlural = [r| d word1, word2, hyphenated-word3 |]

testStrInline0  :: String
testStrInline0 = "d word1 : meaning1; meaning2; meaning3"

testStrInline1 :: String
testStrInline1 = [r|\
d word1 : meaning1; meaning2; meaning3;
  followup notes...

  NB: further commentary. all text to next TimeStamp should be lumped into the 
  meaning
|]

testLog = [r|
08:23:30 λ. d quiescence, quiescent, quiesce
08:24:43 λ. d vouchsafed, 
            another-word
08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal
            second-line
08:38:20 λ. d elegy
08:45:37 λ. d tumbler
08:49:57 λ. d disport
08:56:30 λ. d larder
08:57:29 λ. d wainscot, 
09:12:16 λ. d fender
09:14:12 λ. d bleat 
09:15:48 λ. d dissever
09:24:04 λ. d rhapsody
|]

isIndented = isPrefixOf "           " 

-- 08:58:23 λ. d dahlia : a bushy flower that has the form of a ball of
-- 08:59:03 λ. d poppy : a red or yellow single-cupped, often cleft flower.
-- 09:14:50 λ. d wanton : ill bred
-- 08:48:29 λ. d loth : loathe
-- Parse entries: from timestamp to timestamp

-- group entries, where an entry in a timestamp, and any number of indented
-- lines
-- NB: assumes that the first
takeEntryGroup :: [String] -> ([String], [String]) -- (rst, entrygroup)
takeEntryGroup [] = ([], [])
takeEntryGroup (ln:lns)  = (ln :) <$> collectIndented lns


-- | collects next n indented lines. expects first line to be indented
collectIndented :: [String] -> ([String], [String]) -- (rst, collected)
collectIndented [] = ([], [])
collectIndented (ln:lns)
  | isIndented ln = (ln :) <$> collectIndented lns
  | otherwise = ((ln:lns), [])


groupEntries :: [String] -> [[String]]
groupEntries [] = []
groupEntries lns =
  let (rst, grp) = takeEntryGroup lns
   in grp : groupEntries rst

-- | USE THIS to split by entry.
-- E.g., run `logToEntryGrps testLog` to view example log
logToEntryGrps :: String -> [(TimeStamp, [String])]
logToEntryGrps = catMaybes 
               . fmap parseEntry 
               . groupEntries 
               . filter (not.null) 
               . lines

restOfStr :: Parser String
restOfStr = many (noneOf [])
 
any :: Parser String
any = many (noneOf [])
 


--entry :: Parser (Maybe TimeStamp, String)
--entry = (,) <$> optional timestamp <*> restOfStr

skip :: Parser p -> Parser ()
skip p = () <$ p

parseEntry :: [String] -> Maybe (TimeStamp, [String])
parseEntry [] =  Nothing
parseEntry (ln:lns)
  | isIndented ln = error "Entry lacks TimeStamp."
  | otherwise = 
    let x = parse ((,) <$> timestamp <*> restOfStr) ln
        --x :: _
     in toMaybe $ (fmap.fmap) (:lns) x
                    

-- the next `headwords`


-- | Parse headwords from plural def query variant (e.g. "d word1, ...,
--  wordN").
headwords :: String -> [String]
headwords = splitOn ", " . dropQueryPrefix

headwords' :: Parser (TimeStamp, String)
headwords' = (fmap.fmap) dropQueryPrefix $ (,) <$> timestamp <*> any --sepBy (many $ noneOf ",") (symbol ",")

mkDefQuery :: [String] -> Maybe DefQuery
mkDefQuery [] = Nothing
mkDefQuery (w:[]) = Just $ Single w
mkDefQuery xs = Just $ Plural xs

toDefQuery :: Parser (Maybe DefQuery)
toDefQuery = fmap mkDefQuery $ char 'd' -- TODO case match on prefix (d vs dvs vs read, etc..)
                             *> space 
                             *> sepBy (many $ noneOf ",") (symbol ",")

-- WIP, current 
parseDefQueries :: String -> [(TimeStamp, Maybe DefQuery)]
parseDefQueries = (fmap.fmap) (join . toMaybe . parse toDefQuery . intercalate ", ") 
                . logToEntryGrps


v0 = "08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal"
v1 = "08:38:20 λ. d elegy"
v2 = "09:24:04 λ. d rhapsody : meaning1; meaning2;..."



data MediaType = Play
               | Book
               | ShortStory
               | Other
               deriving (Eq, Show)


type Title = T.Text

data Author = Author { firstName :: T.Text
                     , lastName  :: T.Text
                     , psuedo    :: Maybe T.Text }
                     deriving (Eq, Show)

type ISBN = T.Text

-- | Q: use enum to distinguish between, e.g., aphorisms, humor, insight,
--      etc.?
type Quotation = T.Text

-- | Represents a piece of textual media.
data Written = Title Author MediaType (Maybe ISBN)
  deriving (Eq, Show)

-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp { hr :: Int
                           , min :: Int
                           , sec :: Int }
                           deriving (Eq, Show)

fromList :: [Int] -> Maybe TimeStamp
fromList (x:(x':(x'':[]))) = Just $ TimeStamp x x' x''
fromList _ = Nothing

type Headword = String
type Meaning = String

newtype Def = Def (Headword, Maybe Meaning) -- ( headword, optional meaning/def)
  deriving (Eq, Show)

-- | Log entry—a.t.m. only for reading logs.
data Entry = Defn Def
           | Writtn Written
           deriving (Eq, Show)

data Log = Log [(TimeStamp, Entry)]
  deriving (Eq, Show)



--type PgNum = Integer


-- ISBN for pgNumbers?

parse :: Parser a -> String -> Result a
parse p = parseString p mempty 

show' :: Show a => a -> IO ()
show' =  pPrint

