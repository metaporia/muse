
{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, TupleSections, QuasiQuotes, FlexibleInstances,
  MultiWayIf, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Time
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides truncated 'UTCTime', called 'UTCTime'', and other time utilites.
-----------------------------------------------------------------------------
module Time where

import Data.Time
import Parse (TimeStamp(..))
import Control.Lens (preview, _Left, _Right)

-- TODO 
-- □  use locale to convert timezone to correct UTC offset (see
-- 'getCurrentTimeZone')
toUTC :: Day -> TimeStamp -> UTCTime
toUTC d (TimeStamp h m s) =
  UTCTime d . secondsToDiffTime . fromIntegral $ hrs + mins + s 
  where hrs = 60 * 60 * h
        mins = 60 * m

-- | Truncates pico seconds from 'DiffTime'.
truncateUTC :: UTCTime -> UTCTime
truncateUTC UTCTime {..} =
  UTCTime {utctDayTime = (secondsToDiffTime . truncate $ utctDayTime), ..}

incrMin :: UTCTime -> UTCTime
incrMin UTCTime {..} = UTCTime {utctDayTime = (secondsToDiffTime 60) + utctDayTime, .. }

left :: Either a b -> Maybe a
left = preview _Left

