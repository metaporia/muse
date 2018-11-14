{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Parse
  --( DefQuery(..)
  --, day'
  --, Entry(..)
  --, entries
  --, entryOrDump
  --, LogEntry(..)
  --, logEntries
  --, parse
  --, timestamp
  --, TimeStamp(..)
  --, toMaybe
  --, relDur
  --, trim
  --, PageNum(..)
  --, PgNum
  --, Attr
  --, RelDur(..)
  --, Quote
  --, Author
  --, Body
  --, Title
  --, parseByteString
  --) where
  where

import Control.Applicative
import Control.Lens.TH (makePrisms)
--import Control.Lens (makeLenses, preview, review)
--import Control.Lens.Tuple
import Control.Monad (void)
import Data.Aeson hiding (Null)
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, intercalate)
--import Data.Maybe (fromJust)
import Data.Time
import GHC.Generics hiding (Prefix, Infix)
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
-- | TODO: triage TODOs!!!!!
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
-- ▣  ignore indentation preserving/setting timestamps (see `Entry::Null`)
-- ▣  dump syntax: (include null-timestamp, perhaps "00:00:00" ?) decide whether to ignore 
--    or ban elipsis for untimestamped entries
--  [r|...
--     [<abbr-ts>] - <activity>
--     ...
--   |]
--   - expansion syntax for, e.g., "read ("A", "B", "C"), by <auth>" where the
--   <auth> attribution ditributes over the titles?
--   - first pass will merely collect the string surrounded by ellipses
-- ▣  from "q<pgNum> \"<quotation>\"
-- ▣  (!!! BLOCKING CLI) tag flattened entries with author, title info inside a structure something
--    like : `(TagDb :: (Tag, [Ts]), [LogEntry])`; where tag maps are fragmented
--    by day, or some other small unit
-- ▣  add pretty show functions for `LogEntry` w word wrap for quotes, etc.
-- ▣  (!!!) add "phrase <phrase>" single line entry variant to capture, e.g.,
--    C. Brontë's "ever and anon" and other choice collocations (like Hailey's
--    "the exhaust of your rage"!)
-- □  add CLI option to suppress `Read` entry output
-- □  parse "dialogue"  of the form:
--    > dialogue
--    > 
--    > <character>: <paragraph>
--    >
--    > <character>: <paragraph>
--    (and so on; consume half of or arbitrarily many character-attributed lines)
-- □  add def/quot/title prefix/infix/suffix search
--    - search quote attributions w/ title & author search strings
-- □  ignore all meta log info, e.g., containing:
--    - "muse"
--    - "muse-pre"
--    - "muse-interim"
-- □  improve error messages
-- □  (!!!) parse n.b.s after all entry types
--    from "(note | N.B.)", containing some specialization
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
-- □  parse "read (book | article | play ) <title>, by <author>" to specify media
--    type; default to "book"?
-- watch [(tv | movie)] <title>[, with <cast-names>, ...,] 
-- □  (?) chapter numbers, for instance, "ch <num"
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
-- □  add comment syntax ("//" | "#" | "--" | "/* ... */") ? pick a few;
--    - distinguish between syntaxes, collect?
--    - where are comments permitted? end of line, dedicated line, or both?
-- □  (!) factor `entryBody` and `newline` discardment out of entry variant parsers
--    and into `entry` (see `emptyLines`)
--    BLOCKED: `entryBody` can't be factored out a.t.m.

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
  = Defn (Maybe PgNum)
         [Headword]
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
trimDefQuery (DefVersus hw m h' m') =
  DefVersus (trim hw) (trim' m) (trim h') (trim' m')

trim' :: String -> [Char]
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
entryBody = untilPNoTs $ void (symbol "...") <|> void timestamp <|> eof

lpad :: Parser a -> Parser a
lpad p = try whiteSpace *> p

rpad :: Parser a -> Parser a
rpad p = p <* whiteSpace

pad :: Parser a -> Parser a
pad = rpad . lpad

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

quot :: Parser ()
quot = void $ pad (char '"')


type Title = String

type Author = String

bookTs :: String
bookTs = "08:23:30 λ. read \"To the Lighthouse\", by Virginia Woolf"

bookTs' :: String
bookTs' = [r|begin to read "To the Lighthouse", by Virginia Woolf |]

emptyLines :: Parser [String]
emptyLines = some . try $ manyTill space newline

unused :: a
unused = undefined
  where
    _ = hr >> min >> sec >> pPrint
    _ = bookTs >> bookTs' >> testLog'

type Quote = String

type Body = String

type Attr = String

data PageNum
  = Page PgNum
  | PStart PgNum
  | PEnd PgNum
  | PFinish PgNum
  deriving (Eq, Show, Generic)

instance ToJSON PageNum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PageNum

testLog' :: String
testLog' =
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

testlog' :: String
testlog' =
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
data RelDur = RelDur
  { yy :: Integer
  , mm :: Integer
  , dd :: Integer
  } deriving (Eq, Show)

index :: [a] -> [(Int, a)]
index xs = zip [len,len - 1 .. 0] xs
  where
    len = length xs - 1

toInteger :: [Integer] -> Integer
toInteger = foldr (\(pow, el) res -> (10 ^ pow) * el + res) 0 . index

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


data SearchType
  = Prefix
  | Infix
  | Suffix
  deriving (Eq, Show, Generic)


searchType :: Parser SearchType
searchType = do
  skipOptional space
  st <-
    const Prefix <$> try (char 'p') <|> const Infix <$> try (char 'i') <|>
    const Suffix <$> char 's'
  space
  return st

testDump' :: String
testDump' = [r|
...
dump aeouoaeu
second line
...

    12:10:01 λ. d sylvan
...    
dump body
multiple lines
... 
   
14:19:00 λ. read "Witches Abroad", by Terry Pratchett
 

|]

day' :: Parser (Maybe Day)
day' = do
  let twoDigits = read <$> count 2 digit
  y <- read . ("20" ++) <$> count 2 digit
  char '.'
  m <- twoDigits
  char '.'
  d <- twoDigits
  return $ fromGregorianValid (fromIntegral y) m d

qo = [r|
08:59:30 λ. quotation

            "There was no treachery too for the world to commit. She knew that.
            No happiness lasted."

            In "To the Lighthouse", by Virginia Woolf

14:19:00 λ. read "Witches Abroad", by Terry Pratchett
|]

austen = [r|

10:54:04 λ. read "Northanger Abbey", by Jane Austen
    10:54:22 λ. q101 

    "To come with a well-informed mind, is to come with the inablity of
    administering to the vanity of others..."

    In "Northanger Abbey" by Jane Austen

    10:55:26 λ. d raillery, coppice, disquisition, dissertation
    13:33:55 λ. d scud, mizzle
    13:36:33 λ. d casement
    13:39:59 λ. q123 
    
    "I cannot speak well enough to be unintelligible."

    In "Northanger Abbey" by Jane Austen
|]

