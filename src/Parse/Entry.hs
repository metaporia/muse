{-# LANGUAGE InstanceSigs, OverloadedStrings, GADTs, QuasiQuotes,
  ScopedTypeVariables, FlexibleInstances, QuasiQuotes, DeriveGeneric,
  TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parse.Entry
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides parsers for `LogEntry` and its constituent types.
--
-- TODO
--
-- □  (!!) store quote as @Quotation Quote (Maybe Attr) (Maybe PgNum)@
--
-- □  add 'Poem' 'Entry' variant
--
-- □  Support entry tags. The proposed tags would be included inside square
-- brackets, e.g., @d 34 [<tag0>, ..., <tagN>] headword : meaning@. A tag
-- sequence should /follow/ the page number, if it's present. Empty tag
-- sequences are permitted but discouraged. Spaces are optional but permissible
-- between entry variant prefix, page number, tag sequence and definition body.
-- Note that the tags themselvel /must/ be non-empty.
-- 
-----------------------------------------------------------------------------
module Parse.Entry where

import Control.Applicative
import Control.Lens.TH (makePrisms)

--import Control.Lens (makeLenses, preview, review)
--import Control.Lens.Tuple
import Control.Monad (void)
import Data.Aeson hiding (Null)
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd, intercalate)
import Data.List.Split (splitOn)

--import Data.Maybe (fromJust)
import Data.Time
import GHC.Generics hiding (Infix, Prefix)
import Helpers
import Helpers
import Parse
import Prelude hiding (min, quot)
import Text.Parser.LookAhead
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Trifecta hiding (Rendering, Span)

-- | Parse an quotation entry (body, without timestamp) of the form:
--
--  > "quotation [<page-number>]
--  >
--  >  <quotation-content>
--  >
--  > <attribution>"
--
--  Meant to store quotable excerpts for retrieval.
--
--  E.g.,
--  >  quotation
--  >
--  >  "There was no treachery too base for the world to commit. She knew
--  >  that..."
--  >
--  >  Mrs. Ramsey in "To the Lighthouse", by Virginia Woolf
--  
--  TODO: conditionally omit capture of attribution when it can be inferred
--  from indentation context.
quotation :: Parser Entry
quotation = do
  _ <- try (lpad $ symbol "quotation") <|> lpad (symbol "q")
  pg <- optional digits
  skipOptional emptyLines
  --q <- lpad $ between quote quote (some $ noneOf "\"")
  q <- lpad $ quote'
  -- FIXME see ~/sputum/muse/19.03.01 for example of valid log the breaks
  -- auto-attribution logic.
  -- TODO discard post quote attribution when indent >= 1
  --titleAuthEtc <- untilPNoTs' $ try (void $ timestamp) <|> try (void $ symbol "...") <|> eof
  titleAuthEtc <- linesNoTs <* skipOptional eof
  return $ Quotation (intercalate " " . fmap trim . lines $ q) (trim titleAuthEtc) pg

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0rvtbf" -- ['\\', '"', ...]
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\r\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

quote' :: Parser String
quote' = do
  char '"'
  s <- many character
  char '"'
  return $ concat s



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

book :: Parser Entry
book = do
  _ <- whiteSpace
  _ <-
    try (symbol "read") <|> try (symbol "begin to read") <|>
    try (symbol "finish reading") <|>
    symbol "finish"
  title <- between quot quot (some $ noneOf "\"")
  _ <- optional $ symbol "," -- 
  _ <- symbol "by" <?> "expected attribution"
  isCanonical <- option False $ symbol "canonical" *> return True
  author <- some (noneOf "\n")
  _ <- entryBody
  _ <- many newline
  return $ Read title author

-- | A tag may contain any (decimal) digit, any classical laten letter, that
-- is, one of [a-z], spaces, underscores, or hyphens. Unicode is /not/
-- supported.
tagChar :: Parser Char
tagChar = try alphaNum <|> oneOf "-_ "

-- | Parses zero or more comma-separated sequences of 'tagChar's within square
-- brackets, e.g., @[tag1,my-tag2,tag_3,some other tag]@. Note that although
-- white space is permitted /within/ tags, leading and trailing spaces are
-- removed.
tags :: Parser [String]
tags = fmap trimTrailing <$> brackets (commaSep (some tagChar))
  where trimTrailing = dropWhileEnd isSpace
      -- we trim after parsing to avoid look-ahead--idk whether it's faster,
      -- but it's feels simlpler to me atm

-- TODO handle "\n    \n" w/o parser fantods
def :: Parser Entry
def = do
  dq <- (try toDefVersus <|> try inlineMeaning <|> toDefn)
  return . Def . trimDefQuery $ dq

-- | Extracts page number as one of: 
--
--  > "p <num>" -- page number 
--  > "s <num>" -- pg at start of reading session 
--  > "e <num>" -- pg at end of reading session 
--  > "f <num>" -- pg at end of book; shorthand for completion of book
page :: Parser Entry
page = do
  _ <- skipOptional (many space)
  -- FIXME parse prefix in same pass as digits: it induces a redundancy
  -- but should avoid the inexhaustive case expression, which seems in poor
  -- taste.
  --
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

-- TODO add test case for ~/sputum/muse/17.10.17, parses only beginning of
-- dump and discards rest of file
dump :: Parser LogEntry
dump =
  let el = string "..."
  in Dump <$> (many space *> el *> manyTill anyChar (try $ newline <* el))

nullE :: Parser Entry
nullE =
  let --x :: _
      x = skipOptional (many space *> (newline <?> "newline here"))
  in const Null <$> x

data LogEntry
  = Dump String
  | TabTsEntry (Int, TimeStamp, Entry)
  deriving (Eq, Show, Generic)

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON LogEntry

logEntry :: Parser LogEntry
logEntry = do
  _ <- skipOptional (try emptyLines)
  let 
      null =  do (indent, ts) <- timestamp'
                 _ <- void (skipOptional spacesNotNewline *> newline) <|> eof
                 return $ TabTsEntry (indent, ts, Null)
      entry' = do
        (indent, ts) <- timestamp' <?> "timestamp"
        e <-
          try book <|> try quotation <|> try commentary <|> try dialogue <|>
          try def <|>
          try page <|>
          try phrase
        return $ TabTsEntry (indent, ts, e)
  e <- try dump <|> try null <|> entry'
  _ <- void (skipOptional emptyLines <?> "emptyLines") <|> eof
  return $ e

tmp = do 
    x <- lpad nullE 
    _ <- void (skipOptional emptyLines) <|> eof
    return x

logEntries :: Parser [LogEntry]
logEntries =
  const [] <$>
  (try (void $ many space) <|> try (void emptyLines) <|> try eof <?> "eat eof") *>
  some logEntry

-- TODO add N.B. field to as many variants as possible (poss. by adding (N.B |
-- n.b. | nota bene) parser to `untilP` in entryBody
data Entry
  = Def DefQuery
  | Read Title
         Author
  | Quotation Quote
              Attr
              (Maybe PgNum)
  | Commentary Body
  | PN PageNum
  | Phr Phrase
  | Dialogue String
  | Null -- ^ represents entry of only a timestamp
  deriving (Eq, Generic, Show)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

isQuotation :: Entry -> Bool
isQuotation (Quotation _ _ _) = True
isQuotation _ = False

data Phrase
  = Plural [Headword]
  | Defined Headword
            Meaning
  deriving (Eq, Show, Generic)

instance ToJSON Phrase where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Phrase

phrase :: Parser Entry
phrase = do
  whiteSpace
  try (symbol "phrase") <|> symbol "phr"
  p <- try definedPhrase <|> pluralPhrase
  many newline
  return $ Phr p

pluralPhrase :: Parser Phrase
pluralPhrase = do
  hws <- sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody
  return $ Plural hws

definedPhrase :: Parser Phrase
definedPhrase = do
  hw <- many (noneOf ":") <* symbol ": "
  meaning <- entryBody
  return $ Defined hw meaning

dialogue :: Parser Entry
dialogue = do
  symbol "dialogue"
  eb <-
    intercalate "\n\n" . fmap (unlines . fmap trim . lines) . splitOn "\n\n" <$>
    entryBody
  return $ Dialogue eb

makePrisms ''Entry

makePrisms ''LogEntry
