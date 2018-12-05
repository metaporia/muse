{-# LANGUAGE FlexibleInstances, OverloadedStrings, RecordWildCards #-}

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
import qualified Data.IxSet
import Data.IxSet (IxSet(..))
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import Data.Set (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (Day, UTCTime(..), toGregorian)
import Data.Time.LocalTime
import Render
import Store (DB(..), Dumps(..), Result(..), getUTC, Results(..))
import Store.Types
import Text.Show.Pretty
import Text.Wrap

instance  ColRender DB where
  colRender col DB{..} = do
    putStr "DB:\n"
    colRender col dumped
    putStrLn "" >> colRender col defs
    putStrLn "" >> colRender col reads
    putStrLn "" >> colRender col quotes
    putStrLn "" >> colRender col dialogues
    putStrLn "" >> colRender col phrases
    putStrLn "" >> colRender col comments
    putStrLn "" >> colRender col chrono

instance (Ord v, ColRender v) => ColRender (IxSet v) where
  colRender col ixSet = colRender col (IxSet.toList ixSet)

instance ColRender Dumps where
  colRender col (Dumps day set) = do
    putStr "Dumps on " 
    colRender col day
    putStr ":\n"
    pPrint (toList set)

instance (ColRender a, ColRender b) => ColRender (TsIdxTup a b) where
  colRender col (TsIdxTup tsIdx) = colRender col tsIdx

instance (ColRender a, ColRender b, ColRender c) => ColRender (TsIdxTrip a b c) where
  colRender col (TsIdxTrip tsIdx) = colRender col tsIdx 

instance (ColRender v) => ColRender (TsIdxTag v) where
  colRender col (TsIdxTag tsIdx tag) =
    colRender col tsIdx >>
    case tag of
      Just (AttrTag utc) -> putStr "         tag: " >> colRender col utc
      Nothing -> return ()

instance ColRender v => ColRender (TsIdx v) where
  colRender col idx =
    colRender col (ts idx) >> colRender col (val idx)

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

instance ColRender Bucket where
  colRender col Dmp = putStr "Dmp"
  colRender col Rds = putStr "Rds"
  colRender col Qts = putStr "Qts"
  colRender col Dial = putStr "Dial"
  colRender col Phrs = putStr "Phrs"
  colRender col (Pgs pn)  = putStr $ "Pgs" <> show pn
  colRender col Cmts = putStr "Cmts"
  colRender col Defs = putStr "Defs"
  colRender col Null = putStr "Null"


fmt indent =
  T.unpack .
  T.unlines .
  fmap ((T.replicate indent " ") <>) .
  wrapTextToLines defaultWrapSettings 79 . T.pack

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
