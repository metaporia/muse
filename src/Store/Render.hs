{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store.Render
-- Copyright   :  2018 Keane Yahn-Kraft
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides pretty rendering logic 'DB'
--
-----------------------------------------------------------------------------
module Store.Render where

import           Control.Monad                  ( (>>) )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import           Data.Time                      ( Day
                                                , UTCTime(..)
                                                , toGregorian
                                                )
import           Data.Time.LocalTime
import           Render
import           Store                          ( Result(..)
                                                , Results(..)
                                                )
import           Text.Wrap

instance (ColRender err, ColRender result) => ColRender (Either err result) where
  colRender col (Left err) = putStr "Error: " >> colRender col err >> putStr "\n"
  colRender col (Right result) = colRender col result

instance (ColRender a, ColRender b) => ColRender (a, b) where
  colRender col (a, b) =
    putStr "(" >> colRender col a >> putStr ", " >> colRender col b >>
    putStrLn ")"

instance (ColRender a, ColRender b, ColRender c) => ColRender (a, b, c) where
  colRender col (a, b, c) =
    putStr "(" >> colRender col a >> putStr ", " >> colRender col b >>
    putStr ", " >>
    colRender col c >>
    putStrLn ")"

-- | Does not print newline after timestamp; renders to localtime.
instance ColRender UTCTime where
  colRender col utc = do
    (ZonedTime (LocalTime day' (TimeOfDay hrs mins secs)) _) <-
      utcToLocalZonedTime utc
    colRender col day'
    putStrLn (" " <> show hrs <> ":" <> show mins <> ":" <> show (truncate secs))

instance ColRender Day where
  colRender col day = putStr (show y <> "-" <> show m <> "-" <> show d)
    where (y, m, d) = toGregorian day

instance ColRender a => ColRender [a] where
  colRender col xs = mapM_ (\x -> colRender col x) xs

fmt indent =
  T.unpack
    . T.unlines
    . fmap ((T.replicate indent " ") <>)
    . wrapTextToLines defaultWrapSettings 79
    . T.pack

instance ColRender Result where
  colRender col (DumpR t) = putStr "Dump:    " >> colRender col t
  colRender col (TsR utc entry) =
    colRender col (utctDay utc) >> putStrLn "" >> colRender col entry

instance ColRender Results where
  colRender col (Results rs)  = go Nothing rs
    where go :: (Maybe UTCTime) -> [Result] -> IO ()
          go _ [] = return ()
          go lastTs (d@(DumpR _):rs) = colRender col d >> go lastTs rs
          go lastTs (ts@(TsR utc entry):rs) =
            let lastDay = utctDay <$> lastTs
                currDay = Just $ utctDay utc
             in case (==) <$> lastDay <*> currDay of
                  Just True -> colRender col entry >> go lastTs rs
                  _ -> colRender col ts >> go (Just utc) rs
