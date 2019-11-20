-----------------------------------------------------------------------------
-- |
-- Module      :  Helpers
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module contains miscellaneous helper functions.
-----------------------------------------------------------------------------
module Helpers where

import           Control.Applicative
import           Data.List                      ( isPrefixOf )
import qualified Data.Text                     as T
import           Text.Show.Pretty
import           Text.Trifecta           hiding ( Rendering
                                                , Span
                                                , render
                                                , rendered
                                                )
import qualified Text.Trifecta.Result          as Tri
import           System.IO                      ( stderr
                                                , hPutStrLn
                                                , hPrint
                                                )

-- | Print 'show' output to stderr.
print' :: Show a => a -> IO ()
print' = hPrint stderr

-- | Pretty print to stderr.
pPrint' :: Show a => a -> IO ()
pPrint' = hPutStrLn stderr . ppShow

double :: a -> (a, a)
double a = (a, a)

-- Like 'maybe' for lists.
list :: b -> ([a] -> b) -> [a] -> b
list emptyCase f [] = emptyCase
list _         f xs = f xs

-- Filters elements, applies function to those which satisfy.
filtermap' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtermap' p f = foldr (\a bs -> if p a then f a : bs else bs) []

-- Filters elements, applies function to those which satisfy.
filtermap :: (a -> Maybe b) -> [a] -> [b]
filtermap f = foldr (\a bs -> maybe bs (: bs) (f a)) []

filterMap = filtermap

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right b) = Just b

maybeToEither :: Maybe a -> Either () a
maybeToEither (Just a) = Right a
maybeToEither Nothing  = Left ()

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure _) = Nothing

-- | Show `ErrInfo`
showErr :: Tri.Result a -> Either String a
showErr (Tri.Success a              ) = Right a
showErr (Tri.Failure (ErrInfo doc _)) = Left $ show doc

periodSep :: Parser String -> Parser [String]
periodSep p = p `sepBy` symbol "."

skipOptDot :: Parser ()
skipOptDot = skipOptional (char '.')

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

isIndented :: String -> Bool
isIndented = isPrefixOf "           "

-- | collects next n indented lines. expects first line to be indented
collectIndented :: [String] -> ([String], [String]) -- (rst, collected)
collectIndented [] = ([], [])
collectIndented (ln : lns) | isIndented ln = (ln :) <$> collectIndented lns
                           | otherwise     = (ln : lns, [])

tilEOL :: Parser String
tilEOL = skipEOL >> some (noneOf "\n")

data MediaType
  = Play
  | Book
  | ShortStory
  deriving (Eq, Show)

data Author' = Author
  { firstName :: T.Text
  , lastName :: T.Text
  , psuedo :: Maybe T.Text
  } deriving (Eq, Show)

type ISBN = T.Text

-- | Represents a piece of textual media.
data Written =
  Title Author'
        MediaType
        (Maybe ISBN)
  deriving (Eq, Show)

type Headword = String

type Meaning = String

--type PgNum = Integer
-- ISBN for pgNumbers?
parse :: Parser a -> String -> Result a
parse p = parseString p mempty

show' :: Show a => a -> IO ()
show' = pPrint

-- | Safe head.
head' :: [a] -> Maybe a
head' []      = Nothing
head' (x : _) = Just x
