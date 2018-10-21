{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Parse where

import Prelude hiding (any, lookup, min, until)

import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Text.Trifecta hiding (rendered, render, Rendering, Span)
import Control.Applicative
import Data.Monoid ((<>))
import Data.Bifunctor
import Data.List (stripPrefix, isPrefixOf, intercalate, dropWhile, dropWhileEnd)
import qualified Data.List.Split as Split
import Text.Trifecta.Result (Result(..))
import Text.Parser.Combinators
import Text.Parser.LookAhead
import qualified Text.Trifecta.Result as Tri
import Data.Maybe (catMaybes)

import Control.Monad.Except
import Control.Monad.State.Class
import Lib

import Data.Maybe (maybe, isJust, fromJust)

import Text.Show.Pretty

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure a) = Nothing

--instance {-# OVERLAPPING #-} Show String where
--  show x = ['"'] ++ x ++ ['"']


-- NB:  See ~/hs-note/src/Parse.hs for trifecta examples.

-- TODO write parsers for the following types from each pattern
-- ▣  from [r|hh:mm:ss λ.|] to TimeStamp
-- ▣  from [r| d <def1>[, <def2>, ..., <defN>\n|] to [Definition]
--    where a <def> is "<word> [: <meaning>]" (the brackets '[' and ']'
--    indicate that the meaning, mapping is optional. The headword, <word>, is
--    not.
-- ▣  from
-- [r| dvs <def1>
--         --- vs ---
--         <def2> |] to  [DefVs]
-- □  (!) from, e.g., [| read "Title", by Author Name\n|] to (Title, Author)
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
--   * ▣  a comma separated query list
--
--     > "d word1[, ..., ]"
--
--      - works over multiple lines
--      - NB: no support for commentary, explication; include such in a
--        separate entry.
--
--   * ▣  for inline definition of headword
--
--     > "d headword : meaning"
--
--   * ▣  headword comparison
--
--     > "dvs headword1 : meaning
--     >      --- vs ---
--     >      headword2 : meaning"
defMultWords :: Parser [Def]
defMultWords = undefined --defQueryPrefix *>


splitByComma = T.split (\c -> c == ',')

--newline :: Parser T.Text
--newline = textSymbol "\n"

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

data DefQuery = Defn [Headword]
              | InlineDef Headword Meaning
              | DefVersus Headword Meaning Headword Meaning
              deriving (Eq, Show)

trimDefQuery :: DefQuery -> DefQuery
trimDefQuery (Defn hws) = Defn (fmap trim hws)
trimDefQuery (InlineDef hw meaning) = InlineDef (trim hw) meaning
trimDefQuery (DefVersus hw m h' m') = DefVersus (trim hw) m (trim h') m'

rmNull :: DefQuery -> DefQuery
rmNull (Defn hws) = Defn (filter (not.null) hws)
rmNull (InlineDef hw meaning) = InlineDef (trim hw) meaning
rmNull (DefVersus hw m h' m') = DefVersus (trim hw) m (trim h') m'


testStrDefn :: String
testStrDefn = [r| d word1, word2, hyphenated-word3 |]

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

08:38:20 λ. d elegy : meaning
08:45:37 λ. d tumbler


eruochaosrecuh

08:49:57 λ. d disport : meaning
08:56:30 λ. d larder
08:57:29 λ. d wainscot
09:12:16 λ. d fender
    09:14:12 λ. d bleat
09:15:48 λ. d dissever
09:24:04 λ. d rhapsody
        aeou
        aeou
        aoeu
        aeou
        aeou
09:15:48 λ. dvs deport : to transport, to carry away, to conduct (refl.)
            --- vs ---
            comport : to endure; carry together; to accord (with)
|]

ts = "aeou\naeou\naeou\n 09:24:04 λ.\n"


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
headwords = Split.splitOn ", " . dropQueryPrefix

headwords' :: Parser (TimeStamp, String)
headwords' = (fmap.fmap) dropQueryPrefix $ (,) <$> timestamp <*> any --sepBy (many $ noneOf ",") (symbol ",")

mkDefQuery :: [String] -> Maybe DefQuery
mkDefQuery [] = Nothing
mkDefQuery xs = Just $ Defn xs


-- TODO DefVs variant parser
toDefn :: Parser DefQuery
toDefn = Defn <$> sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody

-- recent
inlineMeaning :: Parser DefQuery -- InlineDef Headword Meaning
inlineMeaning = InlineDef <$> many (noneOf ":") <* symbol (": ") <*> entryBody

-- recent
inlineMeaning' :: Parser DefQuery -- InlineDef Headword Meaning
inlineMeaning' = InlineDef <$> many (noneOf ":") <* symbol (": ") <*> entryBody



-- | Splits on delimiter
toDefVersus :: Parser DefQuery
toDefVersus = collect <$> p0 <* pad (string "--- vs ---") <*> p1
  where collect (hw,m) (hw',m') = DefVersus hw m hw' m'
        -- it's important that this not parse greedily
        p0 = inlineMeaning' . untilPNoTs $ (string "--- vs ---")
        p1 = inlineMeaning'  entryBody

        inlineMeaning' p = (,) <$> many (noneOf ":") 
                             <*  symbol (": ") 
                             <*> p


        
toEntry :: Parser (TimeStamp, DefQuery)
toEntry = (,) <$> timestamp <* parseDefPrefix <*> fmap trimDefQuery allThree
        

-- WIP, TODO, recent
entries :: Parser [(TimeStamp, DefQuery)]
entries = some (pad toEntry) 

-- more recent
combo :: Parser (DefQuery)
combo = (try inlineMeaning)  <|> toDefn

-- after timestamps
-- parse order: toDefVersus, inlineMeaning, toDefn, toSingle
allThree = (try toDefVersus) <|> combo

parseDefPrefix :: Parser String
parseDefPrefix = (try (string "d "))
                 <|> symbol "dvs"

-- | Collects lines up first occurrence of pattern `p`.
--
--
-- (src)[https://stackoverflow.com/questions/7753959/parsec-error-combinator-many-is-applied-to-a-parser-that-accepts-an-empty-s)
untilP :: Parser p -> Parser String
untilP p = do s <- some (noneOf "\n") <|> return <$> newline
              --hasNewline <- try (const True <$> newline) <|> (const False <$> eof)
              
              s' <- try (lookAhead (lpad p) >> return "") <|> untilP p
              return $ s ++ s'

untilPNoTs :: Parser p -> Parser String
untilPNoTs p = do s <- some (noneOf "\n") <|> return <$> newline
                  s' <- try (lookAhead (lpad timestamp) >> return "")
                        <|>  try (lookAhead (lpad p) >> return "") <|> untilP p
                  return $ s ++ s'
              


-- | Splits entries up by timestamp. From here we need to:
--
--  * parse entries into defs, quots, etc. 
--    N.B: preserve indentation info, as it will be used to group entries
--
-- apply def query parser to strings.
entryStrings :: Parser [String]
entryStrings = filter (\s -> (not.null) s && s /= "\n") <$> many entryBody

entryBody :: Parser String
entryBody = untilP $ (timestamp >> return ()) <|> eof


lpad :: Parser a -> Parser a
lpad p = whiteSpace *> p

rpad :: Parser a -> Parser a
rpad p = p <* whiteSpace

pad = rpad . lpad

rmEmptyQueries :: [(TimeStamp, Maybe DefQuery)] -> [(TimeStamp, DefQuery)]
rmEmptyQueries  = foldr (\(ts, m) rst -> case m of
                                            Just x -> (ts, rmNull $ trimDefQuery x):rst
                                            Nothing -> rst) []

-- NB: The top level parser is going to look like `many entry', where entry
-- consumes a valid variant up to, but not including, the next TS or EOF.



-- TODO trim whitespace from (i) headwords and (ii) second lines of entry
-- groups.
trim = dropWhileEnd isSpace . dropWhile isSpace

trimLogEntry = undefined

v0 = "08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal"
v0' = "prorated, hello, mine, yours, hypochondriacal"
v1 = "08:38:20 λ. d elegy"
v2 = "09:24:04 λ. d rhapsody : meaning1; meaning2;..."
v2' = "rhapsody : meaning1; meaning2;..."
v3' = "lèse majesté : meaning1; meaning2;..."

v3 = [r|
09:24:04 λ. quotation

            "There was no treachery too base for the world to commit. She knew
            that.  Happiness did not last. She knew that."

            Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]

v3'' = [r|quotation

        "There was no treachery too base for the world to commit. She knew
        that.  Happiness did not last. She knew that."

        Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]



-- N.B.: this strips its own prefix, i.e., "quotation\n".
quotation :: Parser (String, String)
quotation = do
  (string "quotation") <* newline
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <- entryBody
  return (q, titleAuthEtc)
    where quot = pad (char '"')

v4 = [r|
dvs headword1 : meaning; aeousrcaheosruhoasuerh
    archoaeusrchaoeush roacheu rahue sarhue achue.
    --- vs ---
    headword2 : meaning; aeosrchu archeoau sraheou.

|]

v5 = [r| deport : to transport, to carry away, to conduct (refl.)
        --- vs ---
        comport : to endure; carry together; to accord (with)
|]

-- | Examples of headwords:
--
-- * "venal" -- [A-Za-z]*
-- * "lèse majesté" -- [A-Za-z ]

skipEOL = skipMany (oneOf "\n")

tilEOL :: Parser String
tilEOL = do
  skipEOL
  val <- some (noneOf "\n")
  return val

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

--type PgNum = Integer


-- ISBN for pgNumbers?

parse :: Parser a -> String -> Result a
parse p = parseString p mempty

show' :: Show a => a -> IO ()
show' =  pPrint

