{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad ((>>))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Show.Pretty
import Render
import Store.Types
import Data.Time (UTCTime(..), toGregorian)
import Data.Time.LocalTime


instance ColRender v => ColRender (TsIdx v) where
  colRender col idx =
    colRender col (ts idx) >> putStr " \"" >> colRender col (val idx) >>
    putStrLn "\""

-- | Does not print newline after timestamp; renders to localtime.
instance ColRender UTCTime where
  colRender col utc = do
    (ZonedTime (LocalTime day' (TimeOfDay hrs mins secs)) _) <-
      utcToLocalZonedTime utc
    let (y, m, d) = toGregorian day'
    putStr (show y <> "." <> show m <> "." <> show d)
    putStr (" " <> show hrs <> ":" <> show mins <> ":" <> show (truncate secs))
