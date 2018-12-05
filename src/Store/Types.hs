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
  ( AttrTag(..)
  , Bucket(..)
  , toBucket
  , mkTsIdx
  , mkTsIdxTup
  , mkTsIdxTup'
  , TsIdx
  , ts
  , val
  , TsIdxTag(..)
  , mkTsIdxTag
  , TsIdxTrip(..)
  , getFst
  , getSnd
  , getThrd
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
import Parse (DefQuery(..), PageNum(..), TimeStamp(..))
import Parse.Entry (Entry(..), LogEntry(..), Phrase(..))

import Time

-- TODO 
-- ▣  (index) replace `[DayLog]` with `IxSet DayLog]`
-- ▣  index by `(Day, TimeStamp)` (primary key?)
-- ▣  index by `Day`


deriving instance Data     PageNum
deriving instance Read     PageNum
deriving instance Ord      PageNum
deriving instance Typeable PageNum
deriveSafeCopy 0 'base ''PageNum

deriving instance Data     DefQuery
deriving instance Read     DefQuery
deriving instance Ord      DefQuery
deriving instance Typeable DefQuery
deriveSafeCopy 0 'base ''DefQuery


-- | Entry variant 'DB' bucket identifier. The original, chronological log
-- order is stored as a set of `(timestamps, tag)` pairs; the tag informs
-- dispatch from which bucket to fetch the assoc'd entry. E.g., if when parsing
-- the 'chrono' set, an entry of the form `(ts, Dmp)` is found, then `ts' will
-- be fetched from `dumped (db :: DB)`.
data Bucket
  = Dmp
  | Rds
  | Qts
  | Dial
  | Phrs
  | Pgs PageNum
  | Cmts
  | Defs
  | Null
  deriving (Eq, Ord, Show)

deriveSafeCopy 0 'base ''Bucket

toBucket :: LogEntry -> Bucket
toBucket (Dump _) = Dmp
toBucket (TabTsEntry (_, _, (Read _ _))) = Rds
toBucket (TabTsEntry (_, _, (Def _))) = Defs
toBucket (TabTsEntry (_, _, (Quotation _ _ _))) = Qts
toBucket (TabTsEntry (_, _, (Commentary _))) = Cmts
toBucket (TabTsEntry (_, _, (PN pg))) = Pgs pg
toBucket (TabTsEntry (_, _, (Phr _))) = Phrs
toBucket (TabTsEntry (_, _, (Dialogue _))) = Dial
toBucket (TabTsEntry (_, _, (Parse.Entry.Null))) = Store.Types.Null

-- | Reference to db's 'reads' bucket by primary key.
newtype AttrTag = AttrTag { attrId :: UTCTime }
  deriving (Data, Eq, Ord, Show, Read)

deriveSafeCopy 0 'base ''AttrTag

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
  
data TsIdxTag v = TsIdxTag
  { tsTag :: TsIdx v
  , attr :: Maybe AttrTag
  } deriving (Data, Eq, Ord, Show, Read)

deriveSafeCopy 0 'base ''TsIdxTag

instance (Ord v, Typeable v) => Indexable (TsIdxTag v) where
  empty =
    ixSet
      [ ixFun $ return . ts . tsTag
      , ixFun $ return . val . tsTag
      , ixFun $ return . attr
      ]

mkTsIdxTag :: UTCTime -> v -> Maybe AttrTag -> TsIdxTag v
mkTsIdxTag ts v tag = TsIdxTag (mkTsIdx ts v) tag

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

getFst = (\(x,_,_) -> x) . val . tsTrip
getSnd = (\(_,x,_) -> x) . val . tsTrip
getThrd = (\(_,_,x) -> x) . val . tsTrip

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

data Defns = Defns { defTs :: TsIdx DefQuery 
                   , defAttr :: Maybe AttrTag }
  deriving (Data, Eq, Ord, Show, Read)

instance Indexable Defns where
  empty = ixSet [ ixFun $ return . ts . defTs ]
