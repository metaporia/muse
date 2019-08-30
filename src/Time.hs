
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

-- | Makes 'UTCTime' from a 'Day' by adding a 'DiffTime' of zero, that is, a
-- time representing midnight on the given day.
dayToUTC :: Day -> UTCTime
dayToUTC day = UTCTime day (secondsToDiffTime 0)

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

incrDay :: Day -> Day
incrDay = succ

incrDayBy :: Int -> Day -> Day
incrDayBy n d = iterate succ d !! n --succ $ incrDayBy (n-1) d

incrUTC :: UTCTime -> UTCTime
incrUTC (UTCTime day dt) = UTCTime (incrDay day) dt
