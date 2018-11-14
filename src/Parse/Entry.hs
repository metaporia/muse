{-# LANGUAGE InstanceSigs, OverloadedStrings, GADTs, QuasiQuotes,
  ScopedTypeVariables, FlexibleInstances, QuasiQuotes, DeriveGeneric,
  TemplateHaskell #-}

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
import GHC.Generics hiding (Prefix, Infix)
import Helpers
import Prelude hiding (min, quot)
import Text.Parser.LookAhead
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Trifecta hiding (Rendering, Span)
import Parse
import Helpers

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
  skipOptional emptyLines
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <-
    try (lookAhead (skipOptional emptyLines <* lpad timestamp) >> return "") <|>
    entryBody
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

-- TODO handle "\n    \n" w/o parser fantods
def :: Parser Entry
def = do
  dq <- (try toDefVersus <|> try inlineMeaning <|> toDefn)
  -- _ <- many $ try (void (some space) <* void newline) <|> void newline <?> "lonely spaces"
  -- _ <- many (try space <* newline) <?> "consume solitary spaces on newline"
  return . Def . trimDefQuery $ dq

--  _ <- many (void newline <|> void (some space) <* void newline)
--  _ <- many $ try (void (some space) <* newline) <|> void newline
--  _ <- emptyLines
-- | Extracts page number as one of form: 
--
--  - "p<num>"  -- page number 
--  - "s<num>"  -- pg at start of reading session 
--  - "e<num>" -- pg at end of reading session 
--  - "f<num>" -- pg at end of book; shorthand for completion of book
page :: Parser Entry
page = do
  _ <- skipOptional (many space)
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
  const Null <$>
  void (skipOptional (many space *> (newline <?> "newline here")))

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
  let entry' = do
        indent <- tabs
        ts <- timestamp <?> "timestamp"
        e <-
          try book <|> try quotation <|> try commentary <|> try dialogue <|>
          try def <|>
          try page <|>
          try phrase <|>
          lpad nullE
        return $ TabTsEntry (indent, ts, e)
  e <- try dump <|> entry'
  _ <- void (skipOptional emptyLines <?> "emptyLines") <|> eof
  return $ e

entry :: Parser (Int, TimeStamp, Entry)
entry = do
  indent <- skipOptional (try emptyLines) *> tabs
  ts <- timestamp <?> "timestamp"
  e <-
    (try book <|> try quotation <|> try commentary <|> try def <|> try page <|>
     return Null -- <?> "found no valid prefix"
     )
  _ <- void (skipOptional emptyLines) <|> eof
  return $ (indent, ts, e)

entries :: Parser [(Int, TimeStamp, Entry)]
entries = some entry <* skipOptional newline

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
  | Phr  Phrase
  | Dialogue String
  | Null -- ^ represents entry of only a timestamp
  deriving (Eq, Generic, Show)

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

isQuotation :: Entry -> Bool
isQuotation (Quotation _ _ _) = True
isQuotation _ = False

data Phrase = Plural [Headword]
            | Defined Headword Meaning
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