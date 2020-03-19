{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  CLI.Parser.Custom
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Custom parsers to apply to optparse-applicative's string
-- arguments.
--
-----------------------------------------------------------------------------
module CLI.Parser.Custom
  ( searchArgument
  , pathToDay
  , reldur
  , caretedPreds
  , parseTags
  )
where

import           CLI.Parser.Types
import           Control.Applicative
import           Control.Lens                   ( _Left
                                                , _Right
                                                , over
                                                , preview
                                                )
import           Control.Monad                  ( join )
import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( isAlpha
                                                , isDigit
                                                )
import           Data.Maybe                     ( isJust )
import qualified Data.Text.Lazy                as TL
import           Data.Time                      ( Day
                                                , fromGregorianValid
                                                )
import           Data.Void                      ( Void )
import           Parse                   hiding ( Parser )
import qualified Parse
import           Parse.Types                    ( DefQueryVariant(..)
                                                , RelDur(..)
                                                )
import           Store.Sqlite                   ( StrSearch(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


----------
-- TAGS --
----------

type Parser' = Parsec Void TL.Text

parseTags :: CLI.Parser.Custom.Parser' [TL.Text]
parseTags = sepBy1 tag (char ',') <* eof
 where
  tag = takeWhile1P Nothing isTagChar
  isTagChar c = isDigit c || isAlpha c || c == '_' || c == '-' || c == '/'

------------------
-- SEARCH PREDS --
------------------

type Parser = Parsec Void String

-- TODO update sqlite queries to all use 'BoolExpr'
caretedPreds :: Parser [String]
caretedPreds = label "^ separated list of strings"
  $ sepBy (takeWhile1P Nothing (\x -> x /= ' ' && x /= '\n')) (char '^')

--- CLI's custom search argument parser.
-- Revised for 'StrSearch' refactor depended on by DefEntry search
-- optimization. See 'selectDefs'' for refactor purpose statement and details.
--
-- TODO revise with 'These' datatype:
--
--   @
--   data These a b = This a | That b | These a b
--   @
--
-- Parses def search query string as specified in the docs for 'defR' but
-- defers search string padding (with "%" characters for SQL's @LIKE@ operator)
-- to the SQL query wrappers (see @filter*@ and 'selectDefs'').
--
-- FIXME with the above deferral
-- FIXME as yet this supports only infix search
searchArgument
  :: Parser
       ( Maybe (BoolExpr (StrSearch String))
       , Maybe (BoolExpr (StrSearch String))
       )
searchArgument =
  bimap Just Just
    <$> try headwordAndMeaning
    <|> ((Nothing, ) . Just <$> try meaningOnly)
    <|> ((, Nothing) . Just <$> headwordOnly)
 where
  colon = lexeme (char ':')
  headwordOnly :: Parser (BoolExpr (StrSearch String))
  headwordOnly = do
    expr <- parseBoolExpr
    optional (try colon)
    eof
    return $ InfixSearch <$> expr
  meaningOnly :: Parser (BoolExpr (StrSearch String))
  meaningOnly = do
    colon
    expr <- parseBoolExpr
    eof
    return $ InfixSearch <$> expr
  headwordAndMeaning
    :: Parser (BoolExpr (StrSearch String), BoolExpr (StrSearch String))
  headwordAndMeaning = do
    hwExpr <- parseBoolExpr
    colon
    meaningExpr <- parseBoolExpr
    eof
    return (InfixSearch <$> hwExpr, InfixSearch <$> meaningExpr)

--- Definition Variant Parser (TODO integrate)
-- | Custom search interface. @muse search CMDS@ will hand @CMDS@ to this
-- for search config extraction.
--
-- Example searches:
--
-- > search d hw:mn2&mn2
--
searchVariant :: Parser DefQueryVariant
searchVariant =
  try (DefVersus' <$ symbol "dvs")
    <|> try (Defn' <$ symbol "d")
    <|> (InlineDef' <$ symbol "inline")
    -- FIXME PHR <|> try (Phrase' <$ symbol "p")

data DefSearchInput
  = DefPred (Maybe (String -> Bool))
            (Maybe (String -> Bool))
  | DefVariants [DefQueryVariant]

instance Show DefSearchInput where
  show = summarize

summarize (DefPred hw mn) =
  "DefPred " <> show (isJust hw) <> " " <> show (isJust mn)
summarize (DefVariants xs) = "DefVariants " <> show xs

--- (new style) SEARCH STRING ARGUMENT PARSER(S)
-- i -> inline def
-- v -> def verus
-- d -> all
--  TODO
defQueryVariants :: Parser String -- [DefQueryVariant]
defQueryVariants = undefined


digits :: Parser Integer
digits = Parse.lexeme L.decimal

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

-- Defaults to '00d00m00y' a.t.m.
ymd :: Parser RelDur
ymd = do
  y <- try year <|> return 0
  m <- try month <|> return 0
  d <- try day <|> return 0
  return $ RelDur y m d

ymd' = RelDur <$> year <*> month <*> day

dmy' = do
  d <- day
  m <- month
  y <- year
  return $ RelDur y m d

mdy = do
  m <- month
  d <- day
  y <- year
  return $ RelDur y m d

myd = RelDur <$> month <*> year <*> day

ym = do
  y <- year
  m <- month
  return $ RelDur y m 0

my = do
  m <- month
  y <- year
  return $ RelDur y m 0

md = do
  m <- month
  d <- day
  return $ RelDur 0 m d

y = do
  y <- year
  return $ RelDur y 0 0

m = do
  m <- month
  return $ RelDur 0 m 0

d = do
  d <- day
  return $ RelDur 0 0 d

yd = do
  y <- year
  d <- day
  return $ RelDur y 0 d

dy = do
  d <- day
  y <- year
  return $ RelDur y 0 d

dm = do
  d <- day
  m <- month
  return $ RelDur 0 m d

--md = (\m d -> RelDur 0 m d) <$> y

reldur :: Parser RelDur
reldur =
  try dmy'
  -- 3
    <|> try ymd'
    <|> try mdy
    <|> try myd
  -- 2
    <|> try ym
    <|> try my

    <|> try md
    <|> try dm

    <|> try yd
    <|> try dy
  -- 1
    <|> try y
    <|> try m
    <|> try d
    <|> return (RelDur 0 0 0)

-- | Parse `RelDur`
relDur :: Parser RelDur
relDur = dmy


day' :: Parser (Maybe Day)
day' = do
  let twoDigits = Parse.twoDigitNatural
  y <- (2000 +) <$> twoDigits
  char '.'
  m <- twoDigits
  char '.'
  d <- twoDigits
  return $ fromGregorianValid (fromIntegral y) m d

-- | Parse basename /not/ absolute path into 'Day'.
pathToDay :: FilePath -> Maybe Day
pathToDay = join . preview _Right . parse day' "day parser"
