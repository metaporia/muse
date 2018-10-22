{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}


module Parse 
  (parse, timestamp, entries, TimeStamp(..))
  
    where

import Prelude hiding (quot, min)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Text.RawString.QQ
import Text.Trifecta hiding (Rendering, Span)
import Control.Applicative
import Data.List (dropWhile, dropWhileEnd)
import Text.Parser.LookAhead

import Text.Show.Pretty (pPrint)

import Control.Monad (void)

import Helpers


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
-- □  from "q<pgNum> " to QuotationLocation
-- □  strip newlines from quotation bodies

-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp { hr :: Int
                           , min :: Int
                           , sec :: Int }
                           deriving (Eq, Show)


skipOptColon :: Parser ()
skipOptColon = skipOptional (char ':')

twoDigit :: Parser Int
twoDigit = read <$> count 2 digit <* skipOptColon

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




data DefQuery = Defn [Headword]
              | InlineDef Headword Meaning
              | DefVersus Headword Meaning Headword Meaning
              deriving (Eq, Show)

trimDefQuery :: DefQuery -> DefQuery
trimDefQuery (Defn hws) = Defn (fmap trim hws)
trimDefQuery (InlineDef hw meaning) = InlineDef (trim hw) meaning
trimDefQuery (DefVersus hw m h' m') = DefVersus (trim hw) m (trim h') m'


toDefn :: Parser DefQuery
toDefn = Defn <$> (symbol "d" *> sepBy (some $ noneOf ",\n") (symbol ",")) <* entryBody


-- recent
inlineMeaning :: Parser DefQuery -- InlineDef Headword Meaning
inlineMeaning = InlineDef <$> (symbol "d" *> many (noneOf ":") <* symbol ": ") <*> entryBody

-- | Splits on delimiter
toDefVersus :: Parser DefQuery
toDefVersus = collect <$> (symbol "dvs" *> p0 <* pad (string "--- vs ---")) <*> p1
  where collect (hw,m) (hw',m') = DefVersus hw m hw' m'
        -- it's important that this not parse greedily
        p0 = inlineMeaning' . untilPNoTs $ string "--- vs ---"
        p1 = inlineMeaning'  entryBody

        inlineMeaning' p = (,) <$> many (noneOf ":") 
                             <*  symbol ": " 
                             <*> p


        

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
untilP p = do s <- some (noneOf "\n") <|> (many newline)
              --nls <- many newline
              --hasNewline <- try (const True <$> newline) <|> (const False <$> eof)
              
              s' <- try (lookAhead (many space *> p) >> return "") <|> untilP p
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

-- N.B.: this strips its own prefix, i.e., "quotation\n".
quotation :: Parser Entry
quotation = do
  _ <- string "quotation" <* newline
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <- entryBody
  _ <- many newline
  return $ Quotation q titleAuthEtc

type Title = String
type Author = String

book :: Parser Entry
book = do
  _ <- try (symbol "read") <|> symbol "begin to read"
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
def =  do
  dq <-  (try toDefVersus 
     <|>  try inlineMeaning 
     <|>  toDefn)

  _ <- many newline
  return . Def . trimDefQuery $ dq

entry :: Parser (Int, TimeStamp, Entry)
entry = do
  -- prefix
  indent <- skipOptional newline *> tabs
  ts <- timestamp

  -- entry body
  dq <-  try book 
     <|> try quotation
     <|> def
  return $ (indent, ts, dq)
  



entries :: Parser [(Int, TimeStamp, Entry)] 
entries = some entry <* skipOptional newline
  where _ = unused

unused :: a
unused = undefined
  where _ = hr >> min >> sec >> pPrint
        _ = bookTs >> bookTs' >> testLog

isQuotation :: Entry -> Bool
isQuotation (Quotation _ _) = True
isQuotation _ = False

type Quote = String
type Attr = String

data Entry = Def DefQuery
           | Read Title Author
           | Quotation Quote Attr
           deriving (Eq, Show)


testLog :: String
testLog = [r|
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
testlog = [r|
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
    10:49:58 λ. quotation
                
                "But nevertheless, the fact remained, that is was nearly
                impossbile to dislike anyone if one looked at them."

                In "To the Lighthouse", by Virginia Woolf

|]
