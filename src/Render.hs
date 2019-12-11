{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, FlexibleInstances, MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Render
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides colorized rendering for `LogEntry` and its constituents.
-----------------------------------------------------------------------------
module Render where

import           Data.Char                      ( toUpper )
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( intercalate )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Parse.Types
import           Parse                          ( trim
                                                , trim'
                                                )
--import           Parse.Entry
import           Prelude                 hiding ( log
                                                , lookup
                                                , min
                                                )
import           System.Console.ANSI
import           Text.Wrap

showAll :: ColRender a => [a] -> IO ()
showAll = traverse_ (colRender True)

indentation :: Int
indentation = 9

indent = replicate indentation ' '

upper = fmap toUpper

applyToRest :: (a -> a) -> [a] -> [a]
applyToRest _ []       = []
applyToRest f (x : xs) = x : fmap f xs

surround :: Char -> String -> String
surround c s = c : (s ++ [c])

colorize :: Bool -> (IO () -> IO ()) -> IO () -> IO ()
colorize True  col = col
colorize False col = id

-- Colorizes IO operation.
mkColor :: Bool -> Color -> IO () -> IO ()
mkColor vivid col io = do
  if vivid
    then setSGR [SetColor Foreground Vivid col]
    else setSGR [SetColor Foreground Dull col]
  io
  setSGR [Reset]

blue = mkColor True Blue

red = mkColor True Red

green = mkColor True Green

magenta = mkColor True Magenta

yellow = mkColor True Yellow

cyan = mkColor True Cyan

dullBlue = mkColor False Blue

dullRed = mkColor False Red

dullGreen = mkColor False Green

dullMagenta = mkColor False Magenta

dullYellow = mkColor False Yellow

dullCyan = mkColor False Cyan

-- Colorizable alternative to `Render`.
class Show a =>
      ColRender a where
  colRender ::
       Bool -- ^ Colorize?
    -> a -- ^ Item to render
    -> IO ()
  render :: a -> IO ()
  render = colRender False

newtype QuoteBody = QuoteBody String
  deriving (Eq, Show)

instance ColRender LogEntry where
  colRender col (Dump s) = putStr "Dump:    " >> colRender col (T.pack s)
  colRender col (TabTsEntry (_, _, entry)) = colRender col entry

instance ColRender Entry where
  colRender col (Def dq) = colRender col dq
  colRender col (Quotation b attr pg) =
    colorize col magenta (putStr "Quote:   ") >>
    colorize col cyan (colRender col . QuoteBody $ surround '"' b) >>
    putStr indent >>
    colorize col yellow (colRender col (T.pack attr)) >>
    putStr indent >>
    colRender col (Page <$> pg) >>
    putStr "\n\n"
  colRender col (Read t a) =
    colorize col yellow (putStrLn $
    "Read:    " ++ (surround '"' t) ++ " by " ++ a)
    >> putStr "\n"
  colRender col (Commentary s) =
    putStr "Comment: " >> colorize col cyan (colRender col (T.pack s))
  colRender col (PN pg) = colRender col pg
  colRender col (Phr p) =
    colorize col magenta (putStr "Phrase:  ") >> colRender col p
  colRender col (Dialogue s) =
    putStr "Dialog.:  \n" >> colorize col cyan (putStrLn $ fmt s)
  colRender _ Null = return ()

instance ColRender DefQuery where
  colRender col (Defn mpg hws) =
    colorize col blue (putStr "Query:   ") >> colRender col mpg >> putStr " " >>
    putStrLn (trim $ " " ++ intercalate ", " hws)
  colRender col (InlineDef hw m) =
    colorize col green (putStr ("Define:  " ++ upper hw ++ ": ")) >>
    colRender col (T.pack m) >> putStrLn ""
  colRender col (DefVersus h m h' m') =
    colorize col red (putStr ("Compare: " ++ upper h ++ ": ")) >>
    colRender col (T.pack m) >>
    putStr ("\n" <> indent ++ "--- vs ---\n" ++ indent) >>
    colorize col red (putStr $ upper h' ++ ": ") >>
    colRender col (T.pack m') >>
    putStrLn ""

instance ColRender Phrase where
  colRender col (Plural ps) =
    colorize col blue $ colRender col (T.pack $ intercalate ", " ps) >> putStrLn ""
  colRender col (Defined p m) =
    colorize col green (putStr $ upper p <> ": ") >> colRender col (T.pack $ trim' m) >> putStrLn ""

instance ColRender QuoteBody where
  colRender col =
    colorize col cyan .
    T.putStr .
    T.unlines .
    applyToRest (T.replicate indentation " " <>) .
    wrapTextToLines defaultWrapSettings 79 . T.pack . \(QuoteBody s) -> s

instance ColRender a => ColRender (Maybe a) where
  colRender col (Just x) = colRender col x
  colRender _ Nothing = return ()


instance ColRender T.Text where
  colRender _ =
    T.putStr .
    T.intercalate "\n" .
    applyToRest (T.replicate indentation " " <>) .
    wrapTextToLines defaultWrapSettings 79

fmt =
  T.unpack
    . T.unlines
    . fmap (T.replicate indentation " " <>)
    . wrapTextToLines defaultWrapSettings 79
    . T.pack

instance ColRender PageNum where
  colRender _ = putStrLn . show

instance ColRender Integer where
  colRender _ = putStr . show

instance ColRender Int where
  colRender _ = putStr . show

instance ColRender Char where
  colRender _ = putStr . show
