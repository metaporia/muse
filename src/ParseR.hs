{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
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
import           Data.Semigroup                 ( (<>) )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
--import           Text.Megaparsec.Debug          ( dbg )

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
quotation :: Parser (Maybe Int, String, Maybe String)
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
  return (pg, body, attr)

-- | Parses quoted content. Assumes that whitespace preceding the opening
-- double quote has been preserved.
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
  symbol' "λ."
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

symbol :: (MonadParsec e s m, Token s ~ Char) => Tokens s -> m (Tokens s)
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol spaceWithoutNewline

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
