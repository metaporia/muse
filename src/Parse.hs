{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Parse
  ( DefQuery(..)
  , Entry(..)
  , entries
  , parse
  , timestamp
  , TimeStamp(..)
  , toMaybe
  , relDur
  , RelDur(..)
  , Quote
  , Author
  , Body
  , Title
  , parseByteString
  ) where

import Control.Applicative
import Control.Monad (void)
import Data.Aeson
import GHC.Generics
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, intercalate)
import Data.Maybe (fromJust)
import Helpers
import Prelude hiding (min, quot)
import Text.Parser.LookAhead
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Trifecta hiding (Rendering, Span)

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
-- ▣  (!) from, e.g., [| read "Title", by Author Name\n|] to (Title, Author)
-- ▣  quotations
-- ▣  strip newlines from quotation bodies
-- □  improve error messages
-- □  from "q<pgNum> " to QuotationLocation
-- □  from "q<pgNum> \"<quotation>\"
-- □  from "(note | N.B.)", containing some specialization
-- □  from "(commentary | synthesis of <title>, by <author>"
--    N.B.: optionally consume attribution info, but prefer to depend upon
--    gruoping of (log) entries by title.
toplevelNote = undefined

-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp
  { hr :: Int
  , min :: Int
  , sec :: Int
  } deriving (Eq, Generic, Show)

instance ToJSON TimeStamp where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimeStamp


skipOptColon :: Parser ()
skipOptColon = skipOptional (char ':')

twoDigit :: Parser Int
twoDigit = read <$> count 2 digit <* skipOptColon

timestamp :: Parser TimeStamp
--timestamp = fromList <$> fmap read <$> periodSep twoDigit
timestamp =
  TimeStamp <$> twoDigit <*> twoDigit <*> twoDigit <* space <* char 'λ' <*
  char '.' <*
  space

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
--
--   * ▣  quotation
--
--     > [r|
--     >  quotation
--     >
--     >  "There was no treachery too base for the world to commit. She knew
--     >  that..."
--     >
--     >  Mrs. Ramsey in "To the Lighthouse", by Virginia Woolf
--     > |]
-- | Examples of headwords:
--
-- * "venal" -- [A-Za-z]*
-- * "lèse majesté" -- [A-Za-z ]
data DefQuery
  = Defn [Headword]
  | InlineDef Headword
              Meaning
  | DefVersus Headword
              Meaning
              Headword
              Meaning
  deriving (Eq, Generic, Show)

instance ToJSON DefQuery where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefQuery


trimDefQuery :: DefQuery -> DefQuery
trimDefQuery (Defn hws) = Defn (fmap trim hws)
trimDefQuery (InlineDef hw meaning) = InlineDef (trim hw) meaning
trimDefQuery (DefVersus hw m h' m') = DefVersus (trim hw) m (trim h') m'

toDefn :: Parser DefQuery
toDefn =
  Defn <$> (symbol "d" *> sepBy (some $ noneOf ",\n") (symbol ",")) <* entryBody

-- recent
inlineMeaning :: Parser DefQuery -- InlineDef Headword Meaning
inlineMeaning =
  InlineDef <$> (symbol "d" *> many (noneOf ":") <* symbol ": ") <*> entryBody

-- | Splits on delimiter
toDefVersus :: Parser DefQuery
toDefVersus =
  collect <$> (symbol "dvs" *> p0 <* pad (string "--- vs ---")) <*> p1
  where
    collect (hw, m) (hw', m') = DefVersus hw m hw' m'
        -- it's important that this not parse greedily
    p0 = inlineMeaning' . untilPNoTs $ string "--- vs ---"
    p1 = inlineMeaning' entryBody
    inlineMeaning' p = (,) <$> many (noneOf ":") <* symbol ": " <*> p

-- | Returns the number of tabs, i.e., 4 spaces. If there are 7 spaces,
--  `tab' "       "` returns `pure 1`.
--
-- We need a "tab" depth parser to determine indentation. To do this, we will
-- count spaces in groups of 4.
tabs :: Parser Int
tabs = length <$> many (try $ count 4 space)

-- | Collects lines up first occurrence of pattern `p`.
--
--
-- (src)[https://stackoverflow.com/questions/7753959/parsec-error-combinator-many-is-applied-to-a-parser-that-accepts-an-empty-s)
untilP :: Parser p -> Parser String
untilP p = do
  s <- some (noneOf "\n") <|> (many newline)
              --nls <- many newline
              --hasNewline <- try (const True <$> newline) <|> (const False <$> eof)
  s' <- try (lookAhead (many space *> p) >> return "") <|> untilP p
  return $ s ++ s'

untilPNoTs :: Parser p -> Parser String
untilPNoTs p = do
  s <- some (noneOf "\n") <|> return <$> newline
  s' <-
    try (lookAhead (lpad timestamp) >> return "") <|>
    try (lookAhead (lpad p) >> return "") <|>
    untilP p
  return $ s ++ s'

-- | Splits entries up by timestamp. From here we need to:
--
--  * parse entries into defs, quots, etc.
--    N.B: preserve indentation info, as it will be used to group entries
entryBody :: Parser String
entryBody = untilP $ void timestamp <|> eof

lpad :: Parser a -> Parser a
lpad p = whiteSpace *> p

rpad :: Parser a -> Parser a
rpad p = p <* whiteSpace

pad :: Parser a -> Parser a
pad = rpad . lpad

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

quot :: Parser ()
quot = void $ pad (char '"')

-- | Parse an quotation entry (body, without timestamp) of the form:
--
--  > "quotation
--  >
--  >  <quotation-content>
--  >
--  > <attribution>"
--
--  Meant to store quotable excerpts for retrieval.
--
--  TODO: conditionally omit capture of attribution when it can be inferred
--  from indentation context.
quotation :: Parser Entry
quotation = do
  _ <- string "quotation" <* newline
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <- entryBody
  _ <- many newline
  return $ Quotation (intercalate " " . fmap trim . lines $ q) titleAuthEtc


-- | Parse an commentary entry (body, without timestamp) of the form:
--
--  > "(commentary | synthesis)
--  >
--  >  <content>"
--
-- Stores synthesis of, or commentary on, some piece of text, or merely the
-- surrounding definitions, quotations, etc.
--
-- TODO: context awareness (see above todo)
commentary :: Parser Entry
commentary = do
  _ <- try (symbol "commentary") <|> symbol "synthesis"
  body <- entryBody
  _ <- many newline
  return . Commentary . unlines . fmap trim . lines $ body




type Title = String

type Author = String

book :: Parser Entry
book = do
  _ <-
    try (symbol "read") <|> try (symbol "begin to read") <|>
    symbol "finish reading"
  title <- between quot quot (some $ noneOf "\"")
  _ <- symbol ","
  _ <- symbol "by"
  author <- some (noneOf "\n")
  _ <- entryBody
  _ <- many newline
  return $ Read title author

bookTs :: String
bookTs = "08:23:30 λ. read \"To the Lighthouse\", by Virginia Woolf"

bookTs' :: String
bookTs' = [r|begin to read "To the Lighthouse", by Virginia Woolf |]

--parseEntry :: Parser (Int, TimeStamp, Entry)
--parseEntry = (,,) <$> (skipOptional newline *> tabs)
--                  <*> timestamp
--                  <*>
def :: Parser Entry
def = do
  dq <- (try toDefVersus <|> try inlineMeaning <|> toDefn)
  _ <- many newline
  return . Def . trimDefQuery $ dq

entry :: Parser (Int, TimeStamp, Entry)
entry
  -- prefix
 = do
  indent <- skipOptional newline *> tabs
  ts <- timestamp
  -- entry body
  dq <- (try book <|> try quotation <|> try commentary <|> def) <?> "found no valid prefix"
  return $ (indent, ts, dq)

entries :: Parser [(Int, TimeStamp, Entry)]
entries = some entry <* skipOptional newline
  where
    _ = unused

unused :: a
unused = undefined
  where
    _ = hr >> min >> sec >> pPrint
    _ = bookTs >> bookTs' >> testLog

isQuotation :: Entry -> Bool
isQuotation (Quotation _ _) = True
isQuotation _ = False

type Quote = String
type Body = String

type Attr = String

data Entry
  = Def DefQuery
  | Read Title
         Author
  | Quotation Quote
              Attr
  | Commentary Body
  deriving (Eq, Generic, Show)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

testLog :: String
testLog =
  [r|
08:23:30 λ. d quiescence, quiescent, quiesce
08:24:43 λ. d vouchsafed, another-word
08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal

08:38:20 λ. d elegy : meaning
08:45:37 λ. d tumbler

08:23:30 λ. begin to read "To the Lighthouse", by Virginia Woolf

08:49:57 λ. d disport : meaning
      gibberish


08:56:30 λ. d larder


08:59:30 λ. quotation

            "There was no treachery too for the world to commit. She knew that.
            No happiness lasted."

            Mrs. Ramsey in "To the Lighthouse", by Virginia Woolf


08:57:29 λ. d wainscot
09:12:16 λ. d fender
        09:14:12 λ. d bleat
        09:15:48 λ. d dissever
        09:24:04 λ. d rhapsody
09:15:48 λ. dvs deport : to transport, to carry away, to conduct (refl.)
            --- vs ---
            comport : to endure; carry together; to accord (with) |]

testlog :: String
testlog =
  [r|
09:55:06 λ. read "To the Lighthouse", by Virginia Woolf
    09:55:17 λ. dvs benignant : kind; gracious; favorable;
                    --- vs ---
                    benign : gentle, mild, or, medically, non-threatening
    10:11:45 λ. dvs malignant : (adj.) disposed to inflict suffering or cause
                distress; inimical; bent on evil.
                    --- vs ---
                    malign : (adj.) having an evil disposition; spiteful;
                    medically trheatening; (v.) to slander; to asperse; to show
                    hatred toward.
    10:17:40 λ. d inimical, traduce, virulent
    10:18:12 λ. d sublime, lintel
    10:24:02 λ. quotation

                "There was no treachery too base for the world to commit. She
                knew this. No happiness lasted."

                In "To the Lighthouse", by Virginia Woolf
    10:25:27 λ. quotation

                "Her simplicity fathomed what clever people falsified."

                In "To the Lighthouse", by Virginia Woolf
    10:28:49 λ. d plover
    10:47:59 λ. d cosmogony
    10:47:59 λ. d -let
    10:49:58 λ. quotation

                "But nevertheless, the fact remained, that is was nearly
                impossbile to dislike anyone if one looked at them."

                In "To the Lighthouse", by Virginia Woolf

|]


-- | Relative duration, conversion from which expects rollover, not clipping,
-- as this is meant as a container for user-entered years, months, and days.
-- Thus, `RelDur 1000 1000 1000` ought to be a valid input to whichever
-- conversion function is used.
data RelDur = RelDur { yy :: Integer
                     , mm :: Integer
                     , dd :: Integer
                     } deriving (Eq, Show)

index :: [a] -> [(Int, a)]
index xs = zip [len, len-1..0] xs
  where len = length xs - 1

toInteger :: [Integer] -> Integer
toInteger = foldr (\(pow, el) res -> (10^pow) * el + res) 0 . index 

digits :: Parser Integer
digits = read <$> some digit

day :: Parser Integer
day = digits <* char 'd'
week :: Parser Integer
week = digits <* char 'w'

month :: Parser Integer
month = digits <* char 'm'

year :: Parser Integer
year = digits <* char 'y'


-- Defaults to '00d00m00y' a.t.m.
dmy :: Parser RelDur
dmy = do
  d <- try day <|> return 0
  m <- try month <|> return 0
  y <- try year <|> return 0
  return $ RelDur y m d

-- | Parse `RelDur`
relDur :: Parser RelDur
relDur = dmy


