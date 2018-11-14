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
import           Parse.Entry
import           Prelude hiding (lookup, log, min)
import           System.Console.ANSI
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
  -- todo add `Phr` variant handling

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

colorize :: Bool -> (IO () -> IO ()) -> IO () -> IO ()
colorize True col = col
colorize False col = id 

-- Colorizes IO operation.
mkColor :: Bool -> Color -> IO () -> IO ()
mkColor vivid col io = do
  if vivid then setSGR [SetColor Foreground Vivid col]
           else setSGR [SetColor Foreground Dull col]
  io
  setSGR [Reset]

blue    = mkColor True Blue
red     = mkColor True Red
green   = mkColor True Green
magenta = mkColor True Magenta
yellow  = mkColor True Yellow
cyan    = mkColor True Cyan

dullBlue    = mkColor False Blue
dullRed     = mkColor False Red
dullGreen   = mkColor False Green
dullMagenta = mkColor False Magenta
dullYellow  = mkColor False Yellow
dullCyan    = mkColor False Cyan

-- Colorizable alternative to `Render`.
class Show a => ColcolRender a where
  colRender :: Bool -- ^ Colorize?
            -> a -- ^ Item to render
            -> IO ()

instance ColcolRender LogEntry where
  colRender col (Dump s) = putStr "Dump:    " >> colRender col s
  colRender col (TabTsEntry (_, _, entry)) = colRender col entry

instance ColcolRender Entry where
  colRender col (Def dq) = colRender col dq
  colRender col (Quotation b attr pg) =
    colorize col magenta (putStr "Quote:   ") >> colorize col cyan (colRender col $ surround '"' b) >>
    putStr indent >>
    colorize col yellow (colRender col attr) >>
    putStr indent >>
    colRender col (Page <$> pg) >>
    putStr "\n"
  colRender col (Read t a) = colorize col yellow . putStrLn $ "Read:    "  ++ (surround '"' t) ++ " by " ++ a
  colRender col (Commentary s) = putStr "Comment: " >> colorize col cyan (colRender col s)
  colRender col (PN pg) = colRender col pg
  colRender col (Phr p) = colorize col magenta (putStr "Phrase:  ") >> colRender col p
  colRender _ Null = return ()

instance ColcolRender DefQuery where
  colRender col (Defn mpg hws) =
    colorize col blue (putStr "Query:   ") >> colRender col mpg >>
    putStrLn (" " ++ intercalate ", " hws)
  colRender col (InlineDef hw m) =
    colorize col green (putStr ("Define:  " ++ upper hw ++ ": ")) >> colRender col m
  colRender col (DefVersus h m h' m') =
    colorize col red (putStr ("Compare: " ++ upper h ++ ": ")) >> colRender col m >>
    putStr (indent ++ "--- vs ---\n" ++ indent) >> colorize col red (putStr $ upper h' ++ ": ") >>
    colRender col m' >> putStrLn ""

instance ColcolRender Phrase where
  colRender col (Plural ps) = colorize col blue $ colRender col (intercalate ", " ps)
  colRender col (Defined p m) = colorize col green  (putStr $ upper p ++ ": ") >> colRender col m

instance ColcolRender a => ColcolRender (Maybe a) where
  colRender col (Just x) = colRender col x
  colRender _ Nothing = return ()


instance ColcolRender [Char] where
  colRender _ =
    putStr .
    T.unpack .
    T.unlines .
    applyToRest ((T.replicate indentation " ") <>) .
    wrapTextToLines defaultWrapSettings 79 . T.pack

instance ColcolRender PageNum where
  colRender _ = putStrLn . show

instance ColcolRender Integer where
  colRender _ = putStr . show

instance ColcolRender Int where
  colRender _ = putStr . show
