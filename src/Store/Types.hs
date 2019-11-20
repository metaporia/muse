{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables, FlexibleInstances, MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store.Types
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Does nothing. Slated for removal.
-----------------------------------------------------------------------------
module Store.Types
  ( AttrTag(..)
  )
where

import           Data.Time

-- | Reference to db's 'reads' bucket by primary key.
newtype AttrTag = AttrTag { attrId :: UTCTime }
  deriving (Eq, Show)
