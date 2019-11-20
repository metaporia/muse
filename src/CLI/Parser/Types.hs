module CLI.Parser.Types
  ( BoolExpr
  , parseBoolExpr
  , interpretBoolExpr
  )
where

import           Control.Applicative
import           Text.Trifecta


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
parseBoolExpr = expr
 where
  expr   = term `chainr1` andE
  term   = factor `chainr1` orE
  factor = parens expr <|> litE
  litE   = LitE <$> token (some letter)
  andE   = AndE <$ symbolic '&'
  orE    = OrE <$ symbolic '|'

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


