{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

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
import qualified Data.ByteString as B
import Data.IxSet
       (Indexable(..), IxSet(..), (@<), (@>), (@=), getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import Search (pathToDay)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Helpers
import Parse
import Parse.Entry
import Prelude hiding (min)
import Render
import Store hiding (Null)
import Store.Render
import Test.Hspec
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Show.Pretty (pPrint)
import Text.Trifecta hiding (render)
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Result as Tri

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

breakage = 
 [ TabTsEntry
      ( 0
      , TimeStamp { hr = 8 , min = 39 , sec = 28 }
      , Read "Catch-22" "Joseph Heller"
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 8 , min = 42 , sec = 14 }
      , Def (Defn Nothing [ "acedia" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 8 , min = 46 , sec = 48 }
      , Def (Defn Nothing [ "jaundice" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 8 , min = 50 , sec = 22 }
      , Def (Defn Nothing [ "matriot" , "patriot" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 1 , sec = 19 }
      , Def
          (Defn
             Nothing [ "infundibuliform" , "vestibular" , "hyomandibular" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 3 , sec = 58 }
      , Def (Defn Nothing [ "putrescent" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 7 , sec = 41 }
      , Def
          (DefVersus
             "supra-"
             "over, or above, but not touching; transcending."
             "super-"
             "exceeding; above; inclusive; on top; superior in size, degree, position, etc.")
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 22 , sec = 17 }
      , Def (Defn Nothing [ "jubilee" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 31 , sec = 11 }
      , Def (Defn Nothing [ "bituminous" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 37 , sec = 43 }
      , Quotation
          "He had decided to live forever or die in the attempt..."
          "In \"Catch-22\" by Joseph Heller"
          Nothing
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 38 , sec = 6 }
      , Def (Defn Nothing [ "flak" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 41 , sec = 51 }
      , Def (Defn Nothing [ "saturnine" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 9 , min = 55 , sec = 16 }
      , Def
          (Defn
             Nothing
             [ "gentian" , "elephantiasis" , "homiletic" , "hortatory" ])
      )
  , TabTsEntry
      ( 0
      , TimeStamp { hr = 16 , min = 28 , sec = 17 }
      , Def (Defn Nothing [ "hagiography" ])
      )
  , TabTsEntry
      ( 0
      , TimeStamp { hr = 16 , min = 40 , sec = 45 }
      , Def (Defn Nothing [ "traint" ])
      )
  , TabTsEntry
      ( 0
      , TimeStamp { hr = 16 , min = 45 , sec = 39 }
      , Def (Defn Nothing [ "stertorous" , "oner" , "noy" ])
      )
  , TabTsEntry
      ( 0
      , TimeStamp { hr = 16 , min = 59 , sec = 22 }
      , Quotation
          "We compensate for our anxiety, insignificance, with egotistic behavior--e.g., by obsessing over the reception of our contributions-- with self-consciousness."
          "Keane Yahn-Krafft"
          Nothing
      )
  , TabTsEntry
      ( 0
      , TimeStamp { hr = 17 , min = 0 , sec = 14 }
      , Read "test" "Author"
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 17 , min = 0 , sec = 33 }
      , Def (Defn Nothing [ "fop" , "aide" , "ey" , "lapis" , "vamp" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 17 , min = 10 , sec = 53 }
      , Def (Defn Nothing [ "quaints" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 17 , min = 17 , sec = 45 }
      , Def (Defn Nothing [ "dado" , "emu" , "aieee" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 17 , min = 20 , sec = 52 }
      , Def (Defn Nothing [ "drinkle" ])
      )
  , TabTsEntry
      ( 1
      , TimeStamp { hr = 17 , min = 22 , sec = 57 }
      , Def
          (Defn
             Nothing
             [ "gar" , "qa" , "cit" , "ragu" , "wen" , "sadist" , "id" ])
      )
  ]

--
--  describe "keyword \"dialogue\"" $ do
--    it "parse logEntries tDialogue" $
--      example $ do
--        (Nothing) `shouldBe` (Just tDialogueOut)
--

withNewConnFrom :: FilePath -> (AcidState DB -> IO ()) -> IO ()
withNewConnFrom fp action =
  bracket
    (openLocalStateFrom fp initDB)
    closeAcidState
    (\acid -> update acid ReinitDB >> action acid)

spec :: Spec
spec = do
  around (withNewConnFrom "testState") $ do
    describe "Add day's worth of logs to DB." $
      it "converts and inserts logs to DB; compares with conversion directly to `Results`" $ \acid -> do
        backupDay <- utctDay <$> getCurrentTime
        let day = maybe backupDay id $ pathToDay "18.11.20"
            
        update acid $ AddDay day breakage

        let r' = Results $ fromLogEntry day <$> breakage
        r <- query acid FromDB
--        query acid FromDB>>=colRender True
        r `shouldBe` r'
        return ()

test = do
  day <- utctDay <$> getCurrentTime
  let updates :: AddDay
      updates = AddDay day $ fmap TabTsEntry output
        --foldr (>>) (return ()) $
        --fmap (\x -> addLogEntry day (TabTsEntry x) Nothing) output
  
  backupDay <- utctDay <$> getCurrentTime
  let day = maybe backupDay id $ pathToDay "18.11.20"
      updates' = AddDay day breakage
  bracket
    (openLocalStateFrom "testState" initDB)
    createCheckpointAndClose
    (\acid -> do
       -- PURGE DB
       update acid ReinitDB
       update acid updates'
       --query acid FromDB >>= colRender True)
       db@DB{..} <- query acid ViewDB
       t <- getCurrentTime 
       query acid FromDB>>=colRender True
       --query acid ViewDB>>=pPrint
       let -- x :: _
           x = defs @< t 
       return ()
       )
  return ()
