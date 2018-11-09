{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
   OverloadedStrings, TupleSections, FlexibleInstances, MultiWayIf #-}
module Render where

import           Control.Monad (void, (>=>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char (toUpper)
import           Data.List (intercalate)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Wrap
import           Helpers
import           Parse
import           Prelude hiding (lookup, log, min)
import           Text.Show.Pretty (pPrint)


indentation :: Int
indentation = 9
indent = take indentation (repeat ' ')

upper = fmap toUpper

applyToRest :: (a -> a) -> [a] -> [a]
applyToRest _ [] = []
applyToRest f (x:xs) = x: fmap f xs

surround :: Char -> String -> String
surround c s = c : (s ++ [c])

-- | For pretty user-end rendering of `LogEntry`s and such.
class Render a where
  render :: a -> String

instance Render LogEntry where
  render (Dump s) = "Dump:    " ++ render s
  render (TabTsEntry (_, _, entry)) = render entry

instance Render Entry where
  render (Def dq) = render dq
  render (Quotation b attr pg) =
    "Quote:   " ++ surround '"' (render b) ++ "\n" ++ indent ++ render attr ++ indent ++ render (Page <$> pg) ++ "\n"
  render (Read t a) = "Read:    " ++ surround '"' t ++ " by " ++ a
  render (Commentary s) = show s
  render (PN pg) = render pg
  render Null = ""

instance Render DefQuery where
  render (Defn mpg hws) =
    "Query:   " ++ render mpg ++ " " ++ intercalate ", " hws
  render (InlineDef hw m) = "Define:  " ++ upper hw ++ ": " ++ render m
  render (DefVersus h m h' m') =
    "Compare: " ++
    upper h ++
    ": " ++
    render m ++
    indent ++ "--- vs ---\n" ++ indent ++ upper h' ++ ": " ++ render m'

instance Render a => Render (Maybe a) where
  render (Just x) = render x
  render Nothing = ""


  -- assumse text width of 79
instance Render [Char] where
  render =
    T.unpack .
    T.unlines .
    applyToRest ((T.replicate indentation " ") <>) .
    wrapTextToLines defaultWrapSettings 79 . T.pack

instance Render PageNum where
  render = show

instance Render Integer where
  render = show

instance Render Int where
  render = show
