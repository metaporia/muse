module Helpers where

--import Prelude hiding (any, lookup, min, until)


import           Control.Applicative
import           Data.List (isPrefixOf)
import qualified Data.Text as T
import           Text.Trifecta hiding (rendered, render, Rendering, Span)
import           Text.Show.Pretty
import qualified Text.Trifecta.Result as Tri

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure _) = Nothing

periodSep :: Parser String -> Parser [String]
periodSep p = p `sepBy` (symbol ".")

skipOptDot :: Parser ()
skipOptDot = skipOptional (char '.')


skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


isIndented :: [Char] -> Bool
isIndented = isPrefixOf "           "

-- | collects next n indented lines. expects first line to be indented
collectIndented :: [String] -> ([String], [String]) -- (rst, collected)
collectIndented [] = ([], [])
collectIndented (ln:lns)
  | isIndented ln = (ln :) <$> collectIndented lns
  | otherwise = (ln:lns, [])



tilEOL :: Parser String
tilEOL = do
  skipEOL
  val <- some (noneOf "\n")
  return val

data MediaType = Play
               | Book
               | ShortStory
               | Other
               deriving (Eq, Show)



data Author' = Author { firstName :: T.Text
                     , lastName  :: T.Text
                     , psuedo    :: Maybe T.Text }
                     deriving (Eq, Show)

type ISBN = T.Text


-- | Represents a piece of textual media.
data Written = Title Author' MediaType (Maybe ISBN)
  deriving (Eq, Show)

type Headword = String
type Meaning = String

--type PgNum = Integer


-- ISBN for pgNumbers?

parse :: Parser a -> String -> Result a
parse p = parseString p mempty

show' :: Show a => a -> IO ()
show' =  pPrint

