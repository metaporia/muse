{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Lib
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module merely exports modules "Helpers", "Parse", "Parse.Entry", "Search", and "Render".
-----------------------------------------------------------------------------
module Lib
  ( module Helpers
  , module Parse
  , module Parse.Entry
  , module Render
  , module Search
  ) where

import Helpers
import Parse
import Parse.Entry
import Render
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
