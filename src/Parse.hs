{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}


module Parse 
  (parse, timestamp, entries, TimeStamp(..))
  
    where

import Prelude hiding (quot, min)
import Data.Char (isSpace)
import Text.Trifecta hiding (Rendering, Span)
import Control.Applicative
import Data.List (dropWhile, dropWhileEnd)
import Text.Parser.LookAhead

import Control.Monad (void)

import Helpers


--instance {-# OVERLAPPING #-} Show String where
--  show x = ['"'] ++ x ++ ['"']


-- NB:  See ~/hs-note/src/Parse.hs for trifecta examples.

-- TODO write parsers for the following types from each pattern
-- ▣  from [r|hh:mm:ss λ.|] to TimeStamp
-- ▣  from [r| d <def1>[, <def2>, ..., <defN>\n|] to [Definition]
--    where a <def> is "<word> [: <meaning>]" (the brackets '[' and ']'
--    indicate that the meaning, mapping is optional. The headword, <word>, is
--    not.
-- ▣  from
-- [r| dvs <def1>
--         --- vs ---
--         <def2> |] to  [DefVs]
-- □  (!) from, e.g., [| read "Title", by Author Name\n|] to (Title, Author)
-- □  from "q<pgNum> " to QuotationLocation
-- □


-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp { hr :: Int
                           , min :: Int
                           , sec :: Int }
                           deriving (Eq, Show)


skipOptColon :: Parser ()
skipOptColon = skipOptional (char ':')

twoDigit :: Parser Int
twoDigit = read <$> count 2 digit <* skipOptColon

timestamp :: Parser TimeStamp
--timestamp = fromList <$> fmap read <$> periodSep twoDigit
timestamp = TimeStamp <$> twoDigit
                      <*> twoDigit
                      <*> twoDigit
                      <* space
                      <* char 'λ'
                      <* char '.'
                      <* space

-- | Definition parsing. The following are valid definition query forms:
--
--   * ▣  a comma separated query list
--
--     > "d word1[, ..., ]"
--
--      - works over multiple lines
--      - NB: no support for commentary, explication; include such in a
--        separate entry.
--
--   * ▣  for inline definition of headword
--
--     > "d headword : meaning"
--
--   * ▣  headword comparison
--
--     > "dvs headword1 : meaning
--     >      --- vs ---
--     >      headword2 : meaning"


-- | Examples of headwords:
--
-- * "venal" -- [A-Za-z]*
-- * "lèse majesté" -- [A-Za-z ]




data DefQuery = Defn [Headword]
              | InlineDef Headword Meaning
              | DefVersus Headword Meaning Headword Meaning
              deriving (Eq, Show)

trimDefQuery :: DefQuery -> DefQuery
trimDefQuery (Defn hws) = Defn (fmap trim hws)
trimDefQuery (InlineDef hw meaning) = InlineDef (trim hw) meaning
trimDefQuery (DefVersus hw m h' m') = DefVersus (trim hw) m (trim h') m'


toDefn :: Parser DefQuery
toDefn = Defn <$> sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody

-- recent
inlineMeaning :: Parser DefQuery -- InlineDef Headword Meaning
inlineMeaning = InlineDef <$> many (noneOf ":") <* symbol ": " <*> entryBody

-- | Splits on delimiter
toDefVersus :: Parser DefQuery
toDefVersus = collect <$> p0 <* pad (string "--- vs ---") <*> p1
  where collect (hw,m) (hw',m') = DefVersus hw m hw' m'
        -- it's important that this not parse greedily
        p0 = inlineMeaning' . untilPNoTs $ string "--- vs ---"
        p1 = inlineMeaning'  entryBody

        inlineMeaning' p = (,) <$> many (noneOf ":") 
                             <*  symbol ": " 
                             <*> p


        
toEntry :: Parser (Int, TimeStamp, DefQuery)
toEntry = (,,) <$> (skipOptional newline *> tabs) <*> timestamp <* parseDefPrefix <*> fmap trimDefQuery parseDefQuery
        

-- We need a "tab" depth parser to determine indentation. To do this, we will
-- count spaces in groups of 4.

-- | Returns the number of tabs, i.e., 4 spaces. If there are 7 spaces, 
--  `tab' "       "` returns `pure 1`.
tabs :: Parser Int
tabs = length <$> many (try $ count 4 space) 


-- more recent q
combo :: Parser DefQuery
combo = try inlineMeaning  <|> toDefn

-- after timestamps
-- parse order: toDefVersus, inlineMeaning, toDefn, toSingle
parseDefQuery :: Parser DefQuery
parseDefQuery = try toDefVersus <|> combo

parseDefPrefix :: Parser String
parseDefPrefix = try (string "d ")
                 <|> symbol "dvs"

-- | Collects lines up first occurrence of pattern `p`.
--
--
-- (src)[https://stackoverflow.com/questions/7753959/parsec-error-combinator-many-is-applied-to-a-parser-that-accepts-an-empty-s)
untilP :: Parser p -> Parser String
untilP p = do s <- some (noneOf "\n") <|> return <$> newline
              --hasNewline <- try (const True <$> newline) <|> (const False <$> eof)
              
              s' <- try (lookAhead (lpad p) >> return "") <|> untilP p
              return $ s ++ s'

untilPNoTs :: Parser p -> Parser String
untilPNoTs p = do s <- some (noneOf "\n") <|> return <$> newline
                  s' <- try (lookAhead (lpad timestamp) >> return "")
                        <|>  try (lookAhead (lpad p) >> return "") <|> untilP p
                  return $ s ++ s'
              


-- | Splits entries up by timestamp. From here we need to:
--
--  * parse entries into defs, quots, etc. 
--    N.B: preserve indentation info, as it will be used to group entries

entryBody :: Parser String
entryBody = untilP $ void timestamp <|> eof


lpad :: Parser a -> Parser a
lpad p = whiteSpace *> p

rpad :: Parser a -> Parser a
rpad p = p <* whiteSpace

pad :: Parser a -> Parser a
pad = rpad . lpad

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace



-- N.B.: this strips its own prefix, i.e., "quotation\n".
quotation :: Parser (String, String)
quotation = do
  _ <- string "quotation" <* newline
  q <- between quot quot (some $ noneOf "\"")
  titleAuthEtc <- entryBody
  return (q, titleAuthEtc)
    where quot = pad (char '"')

-- WIP, recent, TODO: include quotation parser
entries :: Parser [(Int, TimeStamp, DefQuery)]
entries = some toEntry <* skipOptional newline
  where _ = unused

unused :: a
unused = undefined
  where _ = quotation
        _ = hr
        _ = min
        _ = sec


