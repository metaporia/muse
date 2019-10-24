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
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( isJust )
import           Parse.Entry                    ( DefQueryVariant(..)
                                                , allDefVariants
                                                )
import           Store.Sqlite                   ( DefSearch(..)
                                                , StrSearch(..)
                                                )
import           Text.Trifecta

--- CLI's custom search argument parser.

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

data DefSearchInput = DefPred (Maybe (String -> Bool)) (Maybe (String -> Bool))
                    | DefVariants [DefQueryVariant]

toDefSearch :: DefSearchInput -> DefSearch
toDefSearch (DefVariants vs ) = DefSearch vs [] []
toDefSearch (DefPred mHw mMn) = DefSearch [] (toList mHw) (toList mMn)
  where toList = maybe [] return

instance Show DefSearchInput where
  show = summarize

summarize (DefPred hw mn) =
  "DefPred " <> show (isJust hw) <> " " <> show (isJust mn)
summarize (DefVariants xs) = "DefVariants " <> show xs

--- (new style) SEARCH STRING ARGUMENT PARSER(S)


-- i -> inline def
-- v -> def verus
-- d -> all
defQueryVariants :: Parser String -- [DefQueryVariant]
defQueryVariants =  undefined

-- | For EBNF-eqsue grammar specification, see documentation of 'defR'.
--
searchArgument :: Parser (Maybe (String -> Bool), Maybe (String-> Bool))
searchArgument =
  bimap Just Just
    <$> (try headwordAndMeaning)
    <|> ((Nothing, ) . Just <$> try meaningOnly)
    <|> ((, Nothing) . Just <$> headwordOnly)
 where
  headwordOnly :: Parser (String -> Bool)
  headwordOnly = do
    expr <- parseBoolExpr
    skipOptional (symbolic ':')
    return $ \s -> interpretBoolExpr (`isInfixOf` s) expr

  meaningOnly :: Parser (String -> Bool)
  meaningOnly = do
    symbolic ':'
    expr <- parseBoolExpr
    return $ \s -> interpretBoolExpr (`isInfixOf` s) expr

  headwordAndMeaning :: Parser (String -> Bool, String -> Bool)
  headwordAndMeaning = do
    hwExpr <- parseBoolExpr
    symbolic ':'
    meaningExpr <- parseBoolExpr
    return
      ( \s -> interpretBoolExpr (`isInfixOf` s) hwExpr
      , \s -> interpretBoolExpr (`isInfixOf` s) meaningExpr
      )

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
searchArgumentRR
  :: Parser
       ( Maybe (BoolExpr (StrSearch String))
       , Maybe (BoolExpr (StrSearch String))
       )
searchArgumentRR =
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

data DefSearch' = DefSearch' [DefQueryVariant] 
                             (Maybe (String -> Bool)) 
                             (Maybe (String -> Bool))


searchArgumentR :: Parser DefSearch'
searchArgumentR =
    try headwordAndMeaning
    <|> try meaningOnly
    <|> headwordOnly
 where
  headwordOnly :: Parser DefSearch'
  headwordOnly = do
    expr <- parseBoolExpr
    skipOptional (symbolic ':')
    return $ DefSearch' allDefVariants
                        (Just $ \s -> interpretBoolExpr (`isInfixOf` s) expr)
                        Nothing

  meaningOnly :: Parser DefSearch'
  meaningOnly = do
    symbolic ':'
    expr <- parseBoolExpr
    return $ DefSearch' [InlineDef', DefVersus']
                        Nothing
                        (Just $ \s -> interpretBoolExpr (`isInfixOf` s) expr)

  headwordAndMeaning :: Parser DefSearch'
  headwordAndMeaning = do
    hwExpr <- parseBoolExpr
    symbolic ':'
    meaningExpr <- parseBoolExpr
    return $ DefSearch'
      [InlineDef', DefVersus']
      (Just $ \s -> interpretBoolExpr (`isInfixOf` s) hwExpr)
      (Just $ \s -> interpretBoolExpr (`isInfixOf` s) meaningExpr)


