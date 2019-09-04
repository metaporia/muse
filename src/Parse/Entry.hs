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
--    This will be done (this is a description of coarse granulariy) as follows:
--      
--      1. (DONE) Update the entry parser of each variant we mean to support tags with
--      the 'tag' parser.
--
--      2. (DONE) Add a tag list (@Maybe [String]@ or @[String]@?) to each entry
--      variant.
--
--      3. (YOU ARE HERE) Complete migration sqlite (write the damnable search function
--      already) and then store tags in each variant's table.
--
--      4. Include CLI subcommand to collect, within a date range, naturally,
--      entries with a certain tag. How to serialize or expose this entries is
--      as yet undetermined.
-- 
-----------------------------------------------------------------------------
module Parse.Entry where

import           Control.Applicative
import           Control.Lens.TH                ( makePrisms )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.State
import           Data.Aeson              hiding ( Null )
import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhile
                                                , dropWhileEnd
                                                , intercalate
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Semigroup                 ( (<>) )
import           Data.Time
import           Debug.Trace                    ( trace )
import           GHC.Generics            hiding ( Infix
                                                , Prefix
                                                )
import           Helpers
import           Helpers
import           Parse
import           Prelude                 hiding ( min
                                                , quot
                                                )
import           Text.Parser.LookAhead
import qualified Text.PrettyPrint.ANSI.Leijen  as Leijen
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import           Text.Trifecta           hiding ( Rendering
                                                , Span
                                                )

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
  _  <- try (lpad $ symbol "quotation") <|> lpad (symbol "q")
  pg <- optional digits
  skipOptional emptyLines
  --q <- lpad $ between quote quote (some $ noneOf "\"")
  q            <- lpad $ quote'
  -- FIXME see ~/sputum/muse/19.03.01 for example of valid log the breaks
  -- auto-attribution logic.
  -- TODO discard post quote attribution when indent >= 1
  --titleAuthEtc <- untilPNoTs' $ try (void $ timestamp) <|> try (void $ symbol "...") <|> eof
  titleAuthEtc <- linesNoTs <* skipOptional eof
  return $ Quotation (intercalate " " . fmap trim . lines $ q)
                     (trim titleAuthEtc)
                     pg

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
-- TODO support page-numbers and tags
commentary :: Parser Entry
commentary = do
  _    <- try (symbol "commentary") <|> symbol "synthesis"
  body <- entryBody
  _    <- many newline
  return . Commentary . unlines . fmap trim . lines $ body

-- TODO support tags??
book :: Parser Entry
book = do
  _ <- whiteSpace
  _ <-
    try (symbol "read")
    <|> try (symbol "begin to read")
    <|> try (symbol "finish reading")
    <|> symbol "finish"
  title       <- between quot quot (some $ noneOf "\"")
  _           <- optional $ symbol "," -- 
  _           <- symbol "by" <?> "expected attribution"
  -- what is this "canonical" business?
  isCanonical <- option False $ symbol "canonical" *> return True
  author      <- some (noneOf "\n")
  _           <- entryBody
  _           <- many newline
  return $ Read title author

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
    try (const Page <$> symbolic 'p')
    <|> try (const PStart <$> symbolic 's')
    <|> try (const PEnd <$> symbolic 'e')
    <|> const PFinish
    <$> symbolic 'f'
  pg <- digits <?> "page digits"
  _  <- entryBody
  _  <- many newline
  return $ PN $ p pg

-- TODO add test case for ~/sputum/muse/17.10.17, parses only beginning of
-- dump and discards rest of file
dump :: Parser LogEntry
dump = Dump <$> (many space *> el *> manyTill anyChar (try $ newline <* el))
  where el = string "..."

nullE :: Parser Entry
nullE =
  const Null <$> skipOptional (many space *> (newline <?> "newline here"))

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
  let null = do
        (indent, ts) <- timestamp'
        _            <- void (skipOptional spacesNotNewline *> newline) <|> eof
        return $ TabTsEntry (indent, ts, Null)
      entry' = do
        (indent, ts) <- timestamp' <?> "timestamp"
        e            <-
          try book
          <|> try quotation
          <|> try commentary
          <|> try dialogue
          <|> try def
          <|> try page
          <|> try phrase
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
  const []
    <$> (   try (void $ many space)
        <|> try (void emptyLines)
        <|> try eof
        <?> "eat eof"
        )
    *>  some logEntry

-- | Like 'logEntry' but this parser will enforce that no two timestamps are
-- duplicate by requiring each to be greater than and not equal to its
-- predecessor.
--
-- If the timestamp of an entry is found to be less than or equal to that which
-- came before it, some helpful error message should be constructed with the
-- location of the issue, and, ideally, some directions--or better yet, an
-- interactive timestamp editing interface--as to how they might correct the
-- issue so that the day in question's 'LogEntry' may parse successfully.
logEntry' :: Maybe TimeStamp -> Parser (LogEntry, Maybe TimeStamp)
logEntry' Nothing = do
  -- No timestamp means its the first entry in the day or a dump.  If a dump
  -- occurs at the start of the file, the parser will proceed without a
  -- timestamp and treat the first non-dump entry is the start of the file
  -- (that is, w.r.t. the timestamp passing logic). Should a dump be situated
  -- in the middle of a series of timestamped entries, the dump will be parsed
  -- normally and the last timestamp parsed will be passed through for
  -- comparision with the very next timestamp to be encountered.
  le <- logEntry
  -- No lookahead required as it's the first timestamp in the day's log.  As
  -- such, this branch cannot fail due to duplicate or unordered timestamps.
  return $ case le of
    Dump       _          -> (le, Nothing)
    TabTsEntry (_, ts, _) -> (le, Just ts)
logEntry' (Just previousTimeStamp) = do
  le <- lookAhead logEntry
  -- 'lookAhead' places the error caret at the end of well-formed input, in
  -- this case on the character of the malformed timestamp or its preceding
  -- whitespace.
  case le of
    Dump _ -> return (le, Just previousTimeStamp)
    TabTsEntry (_, currentTimeStamp, _) ->
      if previousTimeStamp < currentTimeStamp
        then return (le, Just currentTimeStamp)
        else raiseErr $ Err
          ( return
          $ Leijen.vcat
          $ (Leijen.text "Unordered or duplicate timestamps:" :)
      -- $ (Leijen.line :)
          $ fmap Leijen.text
          $ (wordSensitiveLineWrap
              55
              (  "Found current timestamp, t1 = "
              <> show currentTimeStamp
              <> ", and previous timestamp, t0  = "
              <> show previousTimeStamp
              <> " where t0 >= t1, but timestamps MUST be unique and ordered smallest to greatest."
              )
            )
          )
          mempty
          mempty
          mempty

wordSensitiveLineWrap :: Int -> String -> [String]
wordSensitiveLineWrap n s =
  let takeNCharsWorth _ []      = ([], [])
      takeNCharsWorth n (h : t) = if length h < n
        then bimap (h :) id $ takeNCharsWorth (n - length h) t
        else ([], h : t)
      go ws =
        let (ln, rest) = takeNCharsWorth 80 ws
        in  if not (null rest)
              then unwords ln : go rest
              else unwords ln : [unwords rest]
  in  go (words s)


-- | Apply the parser to the initial state. If it fails, return @[]@.
-- Otherwise, recurse with the new state. 
--
-- This should, for example, be able to parse a list of digits and rather
-- efficiently ensure that they are ascending.
many' :: s -> (s -> Parser (a, s)) -> Parser [a]
many' s p = do
  mx <- try (Just <$> p s) <|> return Nothing
  case mx of
    Just (a, s') -> (a :) <$> many' s' p
    Nothing      -> return []

many'' :: s -> (s -> Parser (a, s)) -> Parser [a]
many'' s p = do
  (a, s') <- p s
  (a :) <$> many'' s' p



ascendingDigits :: Int -> Parser (Int, Int)
ascendingDigits previous = do
  current <- (read . return) <$> digit
  skipOptional (many space *> char ',' <* many space)
  if previous < current
    then return (current, current)
    else unexpected "expected ascending comma-separated digits"


-- | Pass in comma-separated digits that /don't/ ascend to see what error
-- message our dear user(s) will find upon trying to parse a log file with
-- duplicate timestamps.
validatedInputExample input =
  parse (many'' 0 ascendingDigits <* (eof <?> "digits must ascend")) -- "1,2,3" 

logEntries' :: Parser [LogEntry]
logEntries' = do
  try (void $ many space) <|> try (void emptyLines) <|> try eof <?> "eat eof"
  some logEntry
 where
  go p = do
    mLe <- try (Just <$> p) <|> return Nothing
    case mLe of
      Just le -> (le :) <$> go p
      Nothing -> return []


--some' :: Maybe s -> (Maybe s -> Parser (a, Maybe s)) -> Parser [a]
--some' initialState parser = do (a, ms) <- parser initialState
--                               (a:) <$> many' ms parser
--

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
isQuotation _                 = False

data DefQueryVariant
  = Phrase'
  | Defn'
  | InlineDef'
  | DefVersus'
  deriving (Eq, Show)

-- | Note that until the much needed refactor in which 'DefQuery' and 'Phrase'
-- are unified under a single type with a tag list (the tags refactor will
-- allow this), the below jank will treat defined phrases as inline
-- definititions.
defHasType :: DefQueryVariant -> Either Phrase DefQuery -> Bool
defHasType InlineDef' (Left (Defined _ _)) = True
defHasType variant    dq                   = variant == defQueryVariant dq

defQueryVariant :: Either Phrase DefQuery -> DefQueryVariant
defQueryVariant (Right (Defn      _ _    )) = Defn'
defQueryVariant (Right (InlineDef _ _    )) = InlineDef'
defQueryVariant (Right (DefVersus _ _ _ _)) = DefVersus'
defQueryVariant (Left  _                  ) = Phrase'

isDefn :: DefQuery -> Bool
isDefn (Defn _ _) = True
isDefn _          = False

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
  tagList <- optional tags -- TODO tag refactor
  p       <- try definedPhrase <|> pluralPhrase
  many newline
  return $ Phr p

pluralPhrase :: Parser Phrase
pluralPhrase = do
  hws <- sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody
  return $ Plural hws

definedPhrase :: Parser Phrase
definedPhrase = do
  hw      <- many (noneOf ":") <* symbol ": "
  meaning <- entryBody
  return $ Defined hw meaning

dialogue :: Parser Entry
dialogue = do
  symbol "dialogue"
  eb <-
    intercalate "\n\n"
    .   fmap (unlines . fmap trim . lines)
    .   splitOn "\n\n"
    <$> entryBody
  return $ Dialogue eb

makePrisms ''Entry

makePrisms ''LogEntry
