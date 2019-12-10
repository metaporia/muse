
{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables, FlexibleInstances, MultiWayIf, RecordWildCards #-}

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
module Time
  ( toUTC
  , dayToUTC
  , fromUTC
  , incrUTCBy
  )
where

import           Data.Time
import           Parse                          ( TimeStamp(..) )

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
 where
  hrs  = 60 * 60 * h
  mins = 60 * m

fromUTC :: UTCTime -> (Day, TimeStamp)
fromUTC (UTCTime day dt) =
  let seconds     = floor $ toRational dt
      (rest, sec) = divMod seconds 60
      (hr  , min) = divMod rest 60
  in  (day, TimeStamp {..})

incrUTCBy :: Int -> UTCTime -> UTCTime
incrUTCBy n (UTCTime d dt) = UTCTime (incrDayBy n d) dt

incrDayBy :: Int -> Day -> Day
incrDayBy = addDays . fromIntegral

--- UNUSED

---- | Truncates pico seconds from 'DiffTime'.
--truncateUTC :: UTCTime -> UTCTime
--truncateUTC UTCTime {..} =
--  UTCTime {utctDayTime = (secondsToDiffTime . truncate $ utctDayTime), ..}
--
--incrMin :: UTCTime -> UTCTime
--incrMin UTCTime {..} =
--  UTCTime {utctDayTime = (secondsToDiffTime 60) + utctDayTime, ..}

