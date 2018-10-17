{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Parse where

import Prelude hiding (lookup)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ
import qualified Data.Text.Lazy as TL
import Text.Trifecta hiding (rendered, render, Rendering, Span)
import Control.Applicative
import Data.Monoid ((<>))
import Data.Bifunctor

import Control.Monad.Except
import Control.Monad.State.Class
import Lib

import Data.Maybe (maybe)

import Text.Show.Pretty


-- NB:  See ~/hs-note/src/Parse.hs for trifecta examples.

-- TODO write parsers for the following types from each pattern
-- □  from [r|hh:mm:ss λ.|] to TimeStamp
-- □  from, e.g., [| read "Title", by Author Name\n|] to (Title, Author)
-- □  from [r| d <def1>[, <def2>, ..., <defN>\n|] to [Definition]
--    where a <def> is "<word> [: <meaning>]" (the brackets '[' and ']'
--    indicate that the meaning, mapping is optional. The headword, <word>, is
--    not.
-- □  from
-- [r| dvs <def1>
--         --- vs ---
--         <def2> |] to  [DefVs]
-- □  from "q<pgNum> " to QuotationLocation
-- □



-- Datatypes



data MediaType = Play
               | Book
               | ShortStory
               | Other
               deriving (Eq, Show)


type Title = T.Text

data Author = Author { firstName :: T.Text
                     , lastName  :: T.Text
                     , psuedo    :: Maybe T.Text }
                     deriving (Eq, Show)

type ISBN = T.Text

-- | Q: use enum to distinguish between, e.g., aphorisms, humor, insight,
--      etc.?
type Quotation = T.Text

-- | Represents a piece of textual media.
data Written = Title Author MediaType (Maybe ISBN)
  deriving (Eq, Show)

-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp { hr :: Int
                           , min :: Int
                           , sec :: Int }
                           deriving (Eq, Show)

type Headword = T.Text
type Meaning = T.Text

newtype Def = Def (Headword, Meaning) -- ( headword, optional meaning/def)
  deriving (Eq, Show)

-- | Log entry—a.t.m. only for reading logs.
data Entry = Defn Def
           | Writtn Written
           deriving (Eq, Show)

data Log = Log [(TimeStamp, Entry)]
  deriving (Eq, Show)



--type PgNum = Integer


-- ISBN for pgNumbers?
