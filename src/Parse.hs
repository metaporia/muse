{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
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

