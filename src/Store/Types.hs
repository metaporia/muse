{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, TupleSections, QuasiQuotes, FlexibleInstances,
  MultiWayIf #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving, DeriveDataTypeable,
  TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store.Types
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Exports smart constructors for 'TsIdx' and 'TsIdxTup' to guarantee 'UTCTime'
-- pico second truncation.
-----------------------------------------------------------------------------
module Store.Types 
  ( mkTsIdx
  , mkTsIdxTup
  , mkTsIdxTup'
  , TsIdx
  , ts
  , val
  , TsIdxTrip(..)
  , mkTsIdxTrip
  , mkTsIdxTrip'
  , TsIdxTup(..))
  where

import Control.Lens (_1, _2, _3, view)
import Data.Data (Data, Typeable)
import Data.SafeCopy
import Data.IxSet (Indexable(..), IxSet(..), (@=), updateIx, ixFun, ixSet, getOne)
import qualified Data.IxSet as IxSet
import Data.Time

import Time

-- TODO 
-- □  (index) replace `[DayLog]` with `IxSet DayLog]`
-- □  index by `(Day, TimeStamp)` (primary key?)
-- □  index by `Day`

-- | Datetime map indexed by tuple, fst, snd. fst should be the primary key.
data TsIdx v = TsIdx
  { ts :: UTCTime
  , val :: v
  } deriving (Eq, Ord, Show, Read, Data)


-- | Smart constructor for 'TsIdx' to maintain truncation of 'UTCTime's pico seconds.
mkTsIdx :: UTCTime -> v -> TsIdx v
mkTsIdx ts v = TsIdx (truncateUTC ts) v

deriveSafeCopy 0 'base ''TsIdx

instance (Ord v, Typeable v) => Indexable (TsIdx v) where
  empty = ixSet [ixFun $ return . ts, ixFun $ return . val]
  -- if input validation (for truncated 'UTCTime' doesn't work, apply 'truncateUTC' here

newtype TsIdxTup a b = TsIdxTup
  { tsIdx :: TsIdx (a, b)
  } deriving (Eq, Ord, Show, Read, Data)


mkTsIdxTup' :: UTCTime -> (a, b) -> TsIdxTup a b 
mkTsIdxTup' ts v = TsIdxTup $ mkTsIdx ts v

mkTsIdxTup :: TsIdx (a, b) -> TsIdxTup a b 
mkTsIdxTup = TsIdxTup 


instance (Ord a, Ord b, Typeable a, Typeable b) => Indexable (TsIdxTup a b) where
  empty =
    ixSet
      [ ixFun $ return . ts . tsIdx
      , ixFun $ return . val . tsIdx -- tuple
      , ixFun $ return . fst . val . tsIdx -- fst
      , ixFun $ return . snd . val . tsIdx -- snd
      ]

deriveSafeCopy 0 'base ''TsIdxTup

newtype TsIdxTrip a b c = TsIdxTrip
  { tsTrip :: TsIdx (a, b, c)
  } deriving (Eq, Ord, Show, Read, Data)

mkTsIdxTrip' :: UTCTime -> (a, b, c) -> TsIdxTrip a b c
mkTsIdxTrip' ts v = TsIdxTrip $ mkTsIdx ts v

mkTsIdxTrip :: TsIdx (a, b, c) -> TsIdxTrip a b c
mkTsIdxTrip = TsIdxTrip

deriveSafeCopy 0 'base ''TsIdxTrip

instance (Ord a, Ord b, Typeable a, Typeable b, Ord c, Typeable c) =>
         Indexable (TsIdxTrip a b c) where
  empty =
    ixSet
      [ ixFun $ return . ts . tsTrip
      , ixFun $ return . val . tsTrip -- tuple
      , ixFun $ return . view _1 . val . tsTrip -- fst
      , ixFun $ return . view _2 . val . tsTrip -- snd
      , ixFun $ return . view _3 . val . tsTrip -- third
      ]
