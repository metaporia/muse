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
-- This module merely exports modules "Diff", "Helpers", "Parse", "Parse.Entry", 
-- "Search", and "Render".
--
-- TODO
--
-- □  create absolute paths to entries by appending log file name and entry
--    timestamp; then store log of review times of entry ids, w type of entry;
--    this will ease efficient review of quotes.
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
