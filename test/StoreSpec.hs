{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  StoreSpec
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Tests data store.
-----------------------------------------------------------------------------
module StoreSpec where

import Control.Exception (bracket)
import Control.Monad ((>=>), void)
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Remote
import Data.IxSet
       (Indexable(..), IxSet(..), (@=), getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Text.Show.Pretty (pPrint)
import Store hiding (Null)
import Helpers
import Parse
import Parse.Entry
import Prelude hiding (min)
import Test.Hspec
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Trifecta
import qualified Text.Trifecta.Result as Tri

--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
spec :: Spec
spec = do
  describe "keyword \"dialogue\"" $ do
    it "parse logEntries tDialogue" $
      example $ do
        (Nothing) `shouldBe` (Just tDialogueOut)

tDialogueOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 35, sec = 27}
      , Read "Great Expectations" "Charles Dickens")
  ]

tPhraseOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 55, sec = 26}
      , Def
          (Defn Nothing ["raillery", "coppice", "disquisition", "dissertation"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non " "an essential condition"))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non" "an essential condition"))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Def (Defn Nothing ["casement"]))
  ]

testLogWithDumpOutput' :: [LogEntry]
testLogWithDumpOutput' =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 55, sec = 17}
      , Def
          (DefVersus
             "benignant"
             "kind; gracious; favorable;"
             "benign"
             "gentle, mild, or, medically, non-threatening"))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 45}
      , Def
          (DefVersus
             "malignant"
             "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
             "malign"
             "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Def (Defn (Just 38) ["inimical", "traduce", "virulent"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "Her simplicity fathomed what clever people falsified."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

testDumpOutput :: [LogEntry]
testDumpOutput =
  [ Dump "\ndump aeouoaeu\nsecond line"
  , TabTsEntry
      (1, TimeStamp {hr = 12, min = 10, sec = 1}, Def (Defn Nothing ["sylvan"]))
  , Dump "\ndump body\nmultiple lines"
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 14, min = 19, sec = 0}
      , Read "Witches Abroad" "Terry Pratchett")
  ]

testNullOutput :: [(Int, TimeStamp, Entry)]
testNullOutput = [(1, TimeStamp {hr = 12, min = 10, sec = 1}, Null)]

testLonelySpacesOutput :: [(Int, TimeStamp, Entry)]
testLonelySpacesOutput =
  [ (1, TimeStamp {hr = 12, min = 10, sec = 1}, Def (Defn Nothing ["sylvan"]))
  , ( 0
    , TimeStamp {hr = 14, min = 19, sec = 0}
    , Read "Witches Abroad" "Terry Pratchett")
  ]

-- `parse entries teslog`
output :: [(Int, TimeStamp, Entry)]
output =
  [ ( 0
    , TimeStamp {hr = 9, min = 55, sec = 6}
    , Read "To the Lighthouse" "Virginia Woolf")
  , ( 1
    , TimeStamp {hr = 9, min = 55, sec = 17}
    , Def
        (DefVersus
           "benignant"
           "kind; gracious; favorable;"
           "benign"
           "gentle, mild, or, medically, non-threatening"))
  , ( 1
    , TimeStamp {hr = 10, min = 11, sec = 45}
    , Def
        (DefVersus
           "malignant"
           "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
           "malign"
           "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."))
  , ( 1
    , TimeStamp {hr = 10, min = 17, sec = 40}
    , Def (Defn (Just 38) ["inimical", "traduce", "virulent"]))
  , ( 1
    , TimeStamp {hr = 10, min = 18, sec = 12}
    , Def (Defn (Just 38) ["sublime", "lintel"]))
  , ( 1
    , TimeStamp {hr = 10, min = 24, sec = 2}
    , Quotation
        "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
        "In \"To the Lighthouse\", by Virginia Woolf"
        Nothing)
  , ( 1
    , TimeStamp {hr = 10, min = 25, sec = 27}
    , Quotation
        "Her simplicity fathomed what clever people falsified."
        "In \"To the Lighthouse\", by Virginia Woolf"
        Nothing)
  , (1, TimeStamp {hr = 10, min = 28, sec = 49}, Def (Defn Nothing ["plover"]))
  , ( 1
    , TimeStamp {hr = 10, min = 47, sec = 59}
    , Def (Defn Nothing ["cosmogony"]))
  , ( 1
    , TimeStamp {hr = 10, min = 49, sec = 58}
    , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf"
        (Just 38))
  ]
tNullOut =
  [ TabTsEntry (0, TimeStamp {hr = 21, min = 32, sec = 5}, Null)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 22, min = 31, sec = 38}
      , Quotation
          "I am merely coping with the collosal shame of having found out that I exist."
          "Keane Yahn-Krafft"
          Nothing)
  ]

-- TODO add testcase for output
test = do
  day <- utctDay <$> getCurrentTime
  let updates :: AddDay
      updates = AddDay day $ fmap TabTsEntry output
        --foldr (>>) (return ()) $
        --fmap (\x -> addLogEntry day (TabTsEntry x) Nothing) output
  bracket
    (openLocalState initDB)
    createCheckpointAndClose
    (\acid -> do
       update acid updates
       query acid ViewDB >>= pPrint)
  return ()
