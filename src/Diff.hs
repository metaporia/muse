{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diff
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Provides levenshtein distance modified to use sliding window per word in
-- shorter search string--be wary: as such this alg. fails to account for
-- differences in text length due to the padding of short strings.
-----------------------------------------------------------------------------
module Diff where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Metrics (levenshtein)
import Parse
import Parse.Entry

type Tag = String

type TagPred = String -> Bool -- Tag predicate.

-- | Generate a tag for an entry; succeeds for `Read` variant.
--   * extract one tag from title/author attr despite variance
genTag :: Entry -> Maybe Tag
genTag (Read title author) = undefined
  where
    initials = (++ ".") . intercalate "." . fmap (take 1 . trim) . words . trim
genTag _ = Nothing

-- | Trims whitespace, slides shorter string over longer, gets each pair's
-- levenshtein distance, returns shortest.
-- TODO make case-insensitive
textDiff :: T.Text -> T.Text -> Int
textDiff a b =
  let trim = T.unwords . T.words
      a' = trim a
      b' = trim b
      la' = T.length a'
      lb' = T.length b'
  in if | la' == lb' -> levenshtein a' b'
        | la' > lb' -> go lb' b' la' a'
        | otherwise -> go la' a' lb' b'
    -- n.b. breaks if short is longer than long. do not tamper w the above
    -- guard
  where
    go :: Int -> T.Text -> Int -> T.Text -> Int
    go ls short ll long =
      sum $ (\w -> go' (T.length w) w ll long) <$> T.words short
      where
        go' ls short ll long =
          minimum . fmap (uncurry levenshtein) $
          zip (repeat short) $ [T.take ls $ T.drop i long | i <- [0 .. ll - ls]]

cowards :: [T.Text]
cowards = ["Noel Coward", "Noël Coward"]

coward :: [T.Text]
coward = ["Noël Coward"]

eliots :: [T.Text]
eliots =
  [ "George Eliot"
  , "George Elliot"
  , "George Elliot (Mary Ann Evans)"
  , "George Eliot (Mary Ann Evans)"
  ]

eliot :: [T.Text]
eliot = ["George Eliot", "Mary Ann Evans"]

dostoevskys :: [T.Text]
dostoevskys =
  [ "Fyodor Dostoyevsky"
  , "Fyodor Dostoevsky"
  , "Fyodor Dostoevsky"
  , "Fyodor Dostoevski"
  , "Fyodor Dostoevskij"
  , "Fyodor Dostoevskii"
  , "Fyodor Dostoyevski"
  , "Fyodor Dostoyevskij"
  , "Fyodor Dostoevsky"
  , "Dostoyevsky"
  , "Dostoevski"
  , "Dostoevskij"
  , "Dostoevskii"
  , "Dostoyevsky"
  , "Dostoyevski"
  , "Dostoyevskij"
  , "Dostoyevskii"
  , "Dostoyevskii"
  ]

dostoevsky :: [T.Text]
dostoevsky = ["Fyodor Dostoevsky"]

tolstoys :: [T.Text]
tolstoys = ["Leo Tolstoy", "Count Leo Tolstoy", "Tolstoy", "Tlostoy"]

tolstoy :: [T.Text]
tolstoy = ["Leo Tolstoy"]

all' :: [T.Text]
all' = dostoevskys ++ tolstoys ++ eliots ++ cowards

rate :: T.Text -> [T.Text] -> [(Int, T.Text)]
rate canonical = fmap $ \w -> (textDiff canonical w, w)
