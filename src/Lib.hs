{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings #-}

module Lib
  ( module Parse
  , module Search
  , module Helpers
  ) where

import Parse
import Helpers
import Search

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Returns the first successful result.
--choice :: (a -> Maybe b) -> [a] -> Maybe b
--choice f xs = foldr go Nothing xs
--  where go a mb = case mb of
--                    Nothing -> f a
--                    r -> r
--
--newtype P p = P
--  { unP :: (p -> Int) -> 
--           (p -> Bool) ->
--           (String -> Char) ->
--           (Bool -> Char)
--  }
--
--
--instance Functor P where
--  fmap :: (a -> b) -> P a -> P b
--  fmap f (P m) = 
--    let x = m
--     in --undefined 
--       P $ \ int bool -> m (int.f) (bool.f)


