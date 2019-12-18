{-# LANGUAGE OverloadedStrings #-}

module CLI.Parser.Types
  ( BoolExpr(..)
  , parseBoolExpr
  , interpretBoolExpr
  , evalMapBoolExpr
  )
where

import           Control.Applicative
import           Data.Char                      ( isAlpha )
import           Data.Void                      ( Void )
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Monad.Combinators.Expr

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (() <$ takeWhile1P Nothing (== ' ')) empty empty

lexeme = L.lexeme sc

symbol = L.symbol sc

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

litE :: Parser (BoolExpr String)
litE = LitE <$> lexeme (takeWhile1P Nothing isAlpha)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser (BoolExpr String)
term = choice [parens boolExpr, litE]

boolExpr :: Parser (BoolExpr String)
boolExpr = makeExprParser term operatorTable

operatorTable :: [[Operator Parser (BoolExpr String)]]
operatorTable = [[binary "&" AndE, binary "|" OrE]]
  where binary name f = InfixL (f <$ symbol name)

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
parseBoolExpr = boolExpr

--- UNUSED

--boolExprToList :: BoolExpr a -> [a]
--boolExprToList = foldr (:) []

--evalBoolExpr :: (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> BoolExpr a -> b
--evalBoolExpr andE orE litE = go
-- where
--  go expr = case expr of
--    LitE a   -> litE a
--    OrE  l r -> orE (go l) (go r)
--    AndE l r -> andE (go l) (go r)

--interpretBoolExpr' :: BoolExpr Bool -> Bool
--interpretBoolExpr' = interpretBoolExpr id
