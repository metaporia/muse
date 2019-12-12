{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ParseR
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Rewrite of 'Parse' and 'Parse.Entry'.
--
-----------------------------------------------------------------------------
module ParseR where


import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad                  ( void )
import           Data.Char                      ( digitToInt )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Void                      ( Void )
import           Parse.Types                    ( Entry(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.RawString.QQ
--import           Text.Megaparsec.Debug          ( dbg )

---------------------------
-- Commentary & Dialogue --
---------------------------

-- TODO add multiline string literal syntax, e.g., use ``` to delimit raw
-- strings. Otherwise, the commentary must be indented, which messes up the
-- line wrap.
--
-- FIXME indentation-sensitive comment parsing will break, e.g., that 
-- inline-markdown comment on /Carry On, Jeeves/.
commentaryPrefix :: Parser String
commentaryPrefix = cp <?> "commentary prefix"
  where cp = try (symbol "commentary") <|> symbol "synthesis"

commentary :: Parser Entry
commentary = textBlockEntry commentaryPrefix Commentary

dialogue :: Parser Entry
dialogue = textBlockEntry dialoguePrefix Dialogue

dialoguePrefix :: Parser String
dialoguePrefix = cp <?> "dialogue prefix"
  where cp = symbol "dialogue" 


textBlockEntry :: Parser String -> (String -> Entry) -> Parser Entry 
textBlockEntry prefix toEntry = do
  prefix
  many (try emptyLine)
  toEntry <$> textBlock


-- | Parse a text block. A text block may be either indented more than its
-- parent timestamp or fenced with triple-backticks (```).
--
-- Note that this is /not/ backwards-compatible and that older log files will
-- have to be corrected. My hope is that they will be rejected by the new
-- parser loudly and with precision, so ensuring compliance  with the new 
-- spec shouldn't be too much work.
textBlock = try fencedTextBlock <|> fst <$> indentedTextBlock

-- | Parses indented blocks of text. It will succeed with whatever it has when
-- it first encounters insufficiently indented input, so its well-formedness
-- may be reported as a failure the next applied parser.
--
-- Newlines after the block are discarded.
indentedTextBlock :: Parser (String, Int)
indentedTextBlock = do
  let
    lines indent = do
      line       <- many (satisfy (/= '\n'))
      emptyLines <- concat <$> many (try (indentedEmptyLine indent))
      rest       <-
        try
            (  count indent (satisfy (== ' '))
            *> fmap (emptyLines <>) (lines indent)
            )
          <|> pure ""
      return $ line <> rest
  indentLevel <- sum <$> many (1 <$ satisfy (== ' ')) <?> "leading whitespace"
  (, indentLevel) <$> lines indentLevel

-- | A triple-backtick-fenced block of raw text. The fences may be placed
-- directly after the entry variant prefix, on a line of their own, or at the
-- start or end a of line; the three forms /should/ be equivalent. In any case, this 
-- parser expects the first chunk of its input stream to be a fence.
--
-- If present after the opening fence, a single newline dropped.
--
-- Newlines after the block are discarded.
fencedTextBlock :: Parser String
fencedTextBlock = do
  let fence = symbol "```"
  fence <* optional (newline)
  manyTill anySingle fence <* many emptyLine


------------
-- Quotes --
------------

-- Parses a quotation entry body (its timestamp should have been already
-- parsed) at a given indentation level. A quotation entry has the form:
--
--  > (q|quotation) [<page-number>]
--  >
--  > "<quotation-content>"
--  >
--  > [<attribution>]
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
-- Notes:
-- * The entry variant prefix may be @q | quotation@.
-- * The attribution is optional; inferred attributions are preferred in
--   searches.
-- * The entry variant prefix parser is expected not to have leading
--   whitespace.
quotation :: Parser Entry
quotation = do
  quotePrefix
  pg <- optional (lexeme L.decimal)
  many (try emptyLine)
  (body, indentLevel) <- quoteContent
  many (try emptyLine)
  -- consume correct amount of leading whitespace to enforce (continuation) of
  -- correct indentation.
  attr <-
    optional
    $  try
    $  count indentLevel (satisfy isTabOrSpace)
    *> someTill anySingle newline
    <* newline
  return $ Quotation body (fromMaybe "" attr) pg

-- | Parses quoted content. Assumes that whitespace preceding the opening
-- double quote has been preserved.
--
-- FIXME proper tab handling. Debar them or process them correctly.
quoteContent :: Parser (String, Int)
quoteContent = do
  let isDelimiter x = x == '\n' || x == '"'
      lines indent = do
        lineContent  <- many (satisfy (not . isDelimiter)) <?> "line content"
        terminalChar <- satisfy isDelimiter
        case terminalChar of
          '\n' -> do
            emptyLines <- concat <$> many (try (indentedEmptyLine indent))
            count indent (satisfy isTabOrSpace)
            ((emptyLines <> lineContent <> "\n") <>) <$> lines indent
          '"' -> return lineContent
          _   -> error "expected newline or double quote"
  -- N.B. assumes spaces
  indentLevel <-
    sum <$> manyTill (1 <$ satisfy (== ' ')) (char '"') <?> "leading whitespace"
  (, indentLevel) <$> lines indentLevel

quotePrefix :: Parser String
quotePrefix = L.lexeme spaceWithoutNewline qp <?> "quote prefix"
  where qp = try (string "quotation") <|> string "q"


-- Assumes no leading whitespace.
timestamp :: Parser (Int, Int, Int)
timestamp = do
  h <- twoDigitNatural
  char ':'
  m <- twoDigitNatural
  char ':'
  s <- twoDigitNatural
  space1
  symbol "λ."
  return (h, m, s)

-- | Parses a two digit decimal integer.
twoDigitNatural :: Parser Int
twoDigitNatural = do
  tens <- digitToInt <$> digitChar
  ones <- digitToInt <$> digitChar
  return $ tens * 10 + ones

-------------
-- HELPERS --
-------------

type Parser = Parsec Void String

type ParserM e s m = (MonadParsec e s m, Token s ~ Char)

symbol = L.symbol spaceWithoutNewline

lexeme = L.lexeme spaceWithoutNewline

sc :: ParserM e s m => m ()
sc = L.space space1 empty empty

spaceWithoutNewline :: Parser ()
spaceWithoutNewline = L.space (void whitespaceNotNewline) empty empty

whitespaceNotNewline :: Parser String
whitespaceNotNewline = takeWhile1P (Just "tab or space") isTabOrSpace

-- Parse either a newline or a string of tabs and/or spaces followed by
-- newline.
emptyLine :: Parser String
emptyLine =
  (try (return <$> newline) <|> ((++ "\n") <$> whitespaceNotNewline <* newline))
    <?> "empty line"

-- | Like 'emptyLine' but drops the first @indentLevel@ spaces from each
-- line that isn't just a newline.
indentedEmptyLine :: Int -> Parser String
indentedEmptyLine indentLevel = trim indentLevel <$> emptyLine
 where
  trim _ [] = []
  trim 0 xs = xs
  trim n l@(x : xs) | x == ' '  = trim (n - 1) xs
                    | otherwise = l

isTabOrSpace :: Char -> Bool
isTabOrSpace ' '  = True
isTabOrSpace '\t' = True
isTabOrSpace _    = False

----------
-- MISC --
----------

-- indentation-sensitive parsing
indented :: Parser (String, [String]) -- prefix and body line
indented = L.nonIndented sc (L.indentBlock sc p)
 where
  p = do
    prefix <- quotePrefix
    return (L.IndentMany Nothing (return . (prefix, )) (try qItem))

--qItem :: Parser String
qItem = L.lexeme spaceWithoutNewline p
  where p = some (satisfy (\x -> x /= '\n' && x /= '\f' && x /= '\r'))

pt = flip parse ""

curr = "" 
