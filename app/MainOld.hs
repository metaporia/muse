{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, ApplicativeDo, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main (muse)
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the necessaries for muse's CLI.
--
-- TODO 
--
-- ▣  persist with "Data.Acid"
--
-- ▣  serialize w "Data.Serialize" and "SafeCopy"
--
-- □  include locale time zone per file. for midday timezone change, create
--    extra log file for each, with special name. add name handling .
--
-- □  convert inline defs, phrases with meanings to anki card *.txt format
--    □  vim helper: generate cloze from selected quote
--
-----------------------------------------------------------------------------
module MainOld where

import qualified Lib

main = Lib.mainOld
