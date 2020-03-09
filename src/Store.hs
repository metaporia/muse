{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables, FlexibleInstances,
  MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Defines persistence-related types (used mostly by 'Store.Sqlite')
--
-- TODO
--
-- □   (!!) remove redundant 'toLower' calls form poorly executed
--     case-insensitivity refactor.
--
-- □  egad! timezone protection.
--
-- □  impl; Q: how to handle key overwrites?
--          A: collect overwritten keys (whose values are distinct)
-----------------------------------------------------------------------------
module Store
  ( Result(..)
  , Results(..)
  , ToResult
  , Attribution(..)
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           Parse.Types                    ( Entry(..)
                                                , LogEntry(..)
                                                )
import           Prelude
import           Time                           ( toUTC )


type Title = Text
type Author = Text

-- | Type returned by 'DB' queries. Nearly identical to 'LogEntry'.
data Result
  = DumpR Text
  | TsR UTCTime
        Entry -- ^ N.B. doesn't use 'Entry' variant 'PN'
  deriving (Eq, Show)

getUTC :: Result -> Maybe UTCTime
getUTC (DumpR _  ) = Nothing
getUTC (TsR utc _) = Just utc

fromLogEntry :: Day -> LogEntry -> Result
fromLogEntry _   (Dump       s         ) = DumpR (T.pack s)
fromLogEntry day (TabTsEntry (_, ts, e)) = fromEntry (toUTC day ts) e

class ToResult ctx a where
  toResult :: ctx -> a -> Result

instance ToResult Day LogEntry where
  toResult = fromLogEntry

instance ToResult  UTCTime Entry where
  toResult = fromEntry

newtype DumpWrapper = DumpWrapper { unDumpWrapper :: String } deriving (Eq, Show)

-- dump handling is weird bc we have a list of dumps when we parse the sql
-- row (json, presumably). So what we'll do is map @toResult . DumpWrapper@
-- over the list of dumps, each of which will constitute a result.
instance ToResult ctx DumpWrapper where
  toResult _ = DumpR . T.pack . unDumpWrapper

fromEntry :: UTCTime -> Entry -> Result
fromEntry = TsR

newtype Results = Results
  { results :: [Result]
  } deriving (Eq, Show)

data Attribution = Attribution Title Author

