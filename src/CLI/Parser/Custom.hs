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

import           Control.Applicative
import           Data.Bifunctor                 ( bimap )
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( isJust )
import           Parse.Entry                    ( DefQueryVariant(..), allDefVariants )
import           Store.Sqlite                   ( DefSearch(..) )
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

data DefSearch' = DefSearch' [DefQueryVariant] (Maybe (String -> Bool)) (Maybe (String -> Bool))


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



data BoolExpr a = AndE (BoolExpr a) (BoolExpr a)
                | OrE (BoolExpr a) (BoolExpr a)
                | LitE a
                deriving (Show)

instance Functor BoolExpr where
  fmap f (LitE a) = LitE (f a)
  fmap f (OrE l r) = OrE (fmap f l) (fmap f r)
  fmap f (AndE l r) = AndE (fmap f l) (fmap f r)

instance Foldable BoolExpr where
  foldr f base (AndE l r) = foldr f (foldr f base r) l
  foldr f base (OrE l r) = foldr f (foldr f base r) l
  foldr f base (LitE a) = f a base

evalBoolExpr :: (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> BoolExpr a -> b
evalBoolExpr andE orE litE = go
 where
  go expr = case expr of
    LitE a   -> litE a
    OrE  l r -> orE (go l) (go r)
    AndE l r -> andE (go l) (go r)

-- | @evalMapBoolExpr f andOp orOp litOp@ applies @f@ to all 'LitE' leaves; and
-- @andOp@ and @orOp to all 'AndE' and 'OrE' forks, respectively.
evalMapBoolExpr
  :: (a -> b) -> (c -> c -> c) -> (c -> c -> c) -> (b -> c) -> BoolExpr a -> c
evalMapBoolExpr f andE orE litE = go
 where
  go expr = case expr of
    LitE a   -> litE (f a)
    OrE  l r -> orE (go l) (go r)
    AndE l r -> andE (go l) (go r)

-- | Interpret 'BoolExpr' with AND, OR, and a function to convert the tree's
-- values to 'Bool'.
interpretBoolExpr :: (a -> Bool) -> BoolExpr a -> Bool
interpretBoolExpr pred = evalMapBoolExpr pred (&&) (||) id

interpretBoolExpr' :: BoolExpr Bool -> Bool
interpretBoolExpr' = interpretBoolExpr id


-- | Right-associative parser for boolean expressions. 
--
--  * operators limited to AND and OR
--  * elements are alphabetic strings.
--
-- The intended usage of this function is to extract a users search strings
-- from the command-line and then create a single string predicate out of the
-- 'BoolExpr' with 'boolExprSatisfies'. E.g.,
--
-- > evalMapBoolExpr (`isInfixOf` searchCandidate)
--
parseBoolExpr :: Parser (BoolExpr String)
parseBoolExpr = expr
 where
  expr   = term `chainr1` andE
  term   = factor `chainr1` orE
  factor = parens expr <|> litE
  litE   = LitE <$> token (some letter)
  andE   = AndE <$ symbolic '&'
  orE    = OrE <$ symbolic '|'


