{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parse.Entry
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Custom parsers (Trifecta) to apply to optparse-applicative's string
-- arguments.
--
-----------------------------------------------------------------------------
module CLI.Parser.Custom where

import           CLI.Parser.Types
import           Control.Applicative
import           Data.Bifunctor                 ( bimap )
import           Data.Maybe                     ( isJust )
import           Parse.Types                    ( DefQueryVariant(..) )
import           Store.Sqlite                   ( StrSearch(..) )
import           Text.Trifecta

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
  headwordOnly :: Parser (BoolExpr (StrSearch String))
  headwordOnly = do
    expr <- parseBoolExpr
    skipOptional (symbolic ':')
    return $ InfixSearch <$> expr
  meaningOnly :: Parser (BoolExpr (StrSearch String))
  meaningOnly = do
    symbolic ':'
    expr <- parseBoolExpr
    return $ InfixSearch <$> expr
  headwordAndMeaning
    :: Parser (BoolExpr (StrSearch String), BoolExpr (StrSearch String))
  headwordAndMeaning = do
    hwExpr <- parseBoolExpr
    symbolic ':'
    meaningExpr <- parseBoolExpr
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
  try (Defn' <$ symbolic 'd')
    <|> try (DefVersus' <$ symbol "dvs")
    <|> try (Phrase' <$ symbolic 'p')
    <|> (InlineDef' <$ symbol "inline")

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
