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
  , PageNum(..)
  , PgNum
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

-- N.B. ALL PARSERS must clean up after themseves as in `p <* entryBody <* many newlines`
-- | TODO 
--
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
-- ▣  from "(commentary | synthesis of <title>, by <author>"
--    N.B.: optionally consume attribution info, but prefer to depend upon
-- ▣  deprecate comma before <auth> attribution
-- ▣  page numbers, viz., p<num> | s<num> | e<num> | f<num> (total pagecount) | d<num> <word>
-- □  (!!) factor `entryBody` and `newline` discardment out of entry variant parsers
--    and into `entry`
-- □  improve error messages
-- □  from "q<pgNum> " to QuotationLocation
-- □  from "q<pgNum> \"<quotation>\"
-- □  from "(note | N.B.)", containing some specialization
--    grouping of (log) entries by title.
--
--    Example note at the end of a def. needs label; one of: "N.B.", "ref",
--    etc., or quotation (perhaps add to Quotation a "clarificatory" bool?)
--  [r| 19:32:42 λ. d adverse, averse
-- 
--    "Men have an aversion to what breaks in upon their habits; a reluctance and
--    repugnance to what crosses their will; a disgust at what offends their
--    sensibilities; and are often governed by antipathies for which they can
--    give no good reason." - See {Dislike}
--  |]
-- 
-- □  finish multiple books at once?
-- □  add comment syntax ("//" | "#" | "--" | "/* ... */") ? pick a few;
--    distinguish between syntaxes, collect?
-- □  parse "read (book | article | play ) <title>, by <author>" to specify media
--    type; default to "book"?
-- watch [(tv | movie)] <title>[, with <cast-names>, ...,] 
-- □  ignore indentation preserving/setting timestamps
-- □  (?) chapter numbers, for instance, "ch <num"
-- □  dump syntax: (include null-timestamp, perhaps "00:00:00" ?) decide whether to ignore 
--    or ban elipsis for untimestamped entries
--  [r|...
--     [<abbr-ts>] - <activity>
--     ...
--   |]
--   - expansion syntax for, e.g., "read ("A", "B", "C"), by <auth>" where the
--   <auth> attribution ditributes over the titles?
--
-- □  add "research" keyword?
-- □  add citation/external reference entry type, as in, "see <ref>", "see @<link>"
-- □  (unprefixed?) life log entries
--    □  fix the unprefixed
--    □  parse "do <act>"
--    □  "distract <activity>" (meaningful only when nested inside "do[: ]");
--       support list syntax (semicolons or bullets?)
--    □  custom keywords for frequent actions, e.g., "hap", "walk", "coffee",
--       "eat (breakfast | lunch | dinner | snack) <food-desc>"
--    □  closing timestamp with "done ..."?
-- □  ignore all meta log info, e.g., containing:
--    - "muse"
--    - "muse-pre"
--    - "muse-interim"
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

-- | Collects timestamp of the form "14:19:00 λ. ".
-- N.B. Collect tabs before invoking `timestapm` as it will greedily consume
-- preceeding whitespace.
timestamp :: Parser TimeStamp
timestamp = do 
  h <- lpad twoDigit
  m <- twoDigit
  s <- twoDigit <* space <* char 'λ' <* char '.' <* space -- todo replace `char '.'` with `symbolic '.'`.
  return $ TimeStamp h m s

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
  = Defn (Maybe PgNum) [Headword]
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

type PgNum = Integer

trimDefQuery :: DefQuery -> DefQuery
trimDefQuery (Defn pg hws) = Defn pg (fmap trim hws)
trimDefQuery (InlineDef hw meaning) = InlineDef (trim hw) (trim' meaning)
trimDefQuery (DefVersus hw m h' m') = DefVersus (trim hw) (trim' m) (trim h') (trim' m')

trim' = intercalate " " . fmap trim . lines

toDefn :: Parser DefQuery
toDefn = do
  _ <- symbolic 'd'
  pg <- optional digits
  headwords <- sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody
  return $ Defn pg headwords

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
  _ <- try (lpad $ symbol "quotation") <|> lpad (symbol "q")
  pg <- optional digits
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <- entryBody
  _ <- many newline
  return $ Quotation (intercalate " " . fmap trim . lines $ q) titleAuthEtc pg


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
  _ <- whiteSpace
  _ <-
    try (symbol "read") <|> try (symbol "begin to read") <|>
    try (symbol "finish reading") <|> symbol "finish"
  title <- between quot quot (some $ noneOf "\"")
  _ <- optional $ symbol "," -- 
  _ <- symbol "by" <?> "expected attribution"
  author <- some (noneOf "\n")
  _ <- entryBody
  _ <- many newline
  return $ Read title author

bookTs :: String
bookTs = "08:23:30 λ. read \"To the Lighthouse\", by Virginia Woolf"

bookTs' :: String
bookTs' = [r|begin to read "To the Lighthouse", by Virginia Woolf |]


emptyLines :: Parser [String]
emptyLines = some . try $ manyTill space newline 

--parseEntry :: Parser (Int, TimeStamp, Entry)
--parseEntry = (,,) <$> (skipOptional newline *> tabs)
--                  <*> timestamp
--                  <*>
-- TODO handle "\n    \n" w/o parser fantods
def :: Parser Entry
def = do
  dq <- (try toDefVersus <|> try inlineMeaning <|> toDefn)
  -- _ <- many $ try (void (some space) <* void newline) <|> void newline <?> "lonely spaces"
  -- _ <- many (try space <* newline) <?> "consume solitary spaces on newline"
--  _ <- many (void newline <|> void (some space) <* void newline)
--  _ <- many $ try (void (some space) <* newline) <|> void newline
--  _ <- emptyLines
  return . Def . trimDefQuery $ dq

-- | Extracts page number as one of form: 
--
--  - "p<num>"  -- page number 
--  - "s<num>"  -- pg at start of reading session 
--  - "e<num>" -- pg at end of reading session 
--  - "f<num>" -- pg at end of book; shorthand for completion of book
page :: Parser Entry
page = do
  p <-
    try (symbolic 'p') <|> try (symbolic 's') <|> try (symbolic 'e') <|>
    symbolic 'f'
  pg <- digits <?> "page digits"
  _ <- entryBody
  _ <- many newline
  return . PN $
    case p of
      'p' -> Page pg
      's' -> PStart pg
      'e' -> PEnd pg
      'f' -> PFinish pg

entry :: Parser (Int, TimeStamp, Entry)
entry = do
  --_ <- many $ try (void (some space) <* void newline) <?> "lonely spaces"
  indent <- skipOptional (try emptyLines) *> tabs
  ts <- timestamp  <?> "timestamp"
  -- entry body
  e <- (try book <|> try quotation <|> try commentary <|> try def <|> page) -- <?> "found no valid prefix"
  _ <- void (skipOptional emptyLines) <|> eof
  return $ (indent, ts, e)

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
isQuotation (Quotation _ _ _) = True
isQuotation _ = False

type Quote = String
type Body = String

type Attr = String

data PageNum = Page PgNum
             | PStart PgNum
             | PEnd PgNum
             | PFinish PgNum
             deriving (Eq, Show, Generic)

instance ToJSON PageNum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PageNum

data Entry
  = Def DefQuery
  | Read Title
         Author
  | Quotation Quote
              Attr
              (Maybe PgNum)
  | Commentary Body
  | PN PageNum
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
    10:18:12 λ. d48 sublime, lintel
    10:24:02 λ. quotation

                "There was no treachery too base for the world to commit. She
                knew this. No happiness lasted."

                In "To the Lighthouse", by Virginia Woolf
    10:25:27 λ. quotation

                "Her simplicity fathomed what clever people falsified."

                In "To the Lighthouse", by Virginia Woolf
    10:28:49 λ. d plover
    10:47:59 λ. d -let
    10:49:58 λ. quotation

                "But nevertheless, the fact remained, that is was nearly
                impossbile to dislike anyone if one looked at them."

                In "To the Lighthouse", by Virginia Woolf
  
|]
q = [r|10:49:58 λ. quotation

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

testLonelySpaces :: String
testLonelySpaces = [r|

    12:10:01 λ. d sylvan
    
   
14:19:00 λ. read "Witches Abroad", by Terry Pratchett
 

|]


