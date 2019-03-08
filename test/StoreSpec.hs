{-# LANGUAGE QuasiQuotes, NamedFieldPuns, OverloadedStrings,
  RecordWildCards #-}

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
import Data.Acid.Local (Checkpoint, createCheckpointAndClose)
import Data.Acid.Log
import Data.Acid.Remote
import qualified Data.ByteString as B
import Data.Foldable (foldl')
import Data.IxSet
       (Indexable(..), IxSet(..), Proxy(..), (@<), (@=), (@>), (@>=<=),
        getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as IxSet
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Helpers
import Parse
import Parse.Entry
import Prelude hiding (min, reads)
import Render
import Search (pathToDay)
import Store hiding (Null)
import Store.Render
import Store.Types
       (AttrTag(..), TsIdx(..), TsIdxTag(..), mkTsIdxTag, tsIdx)
import Test.Hspec
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Show.Pretty (pPrint)
import Text.Trifecta hiding (render)
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Result as Tri
import Time
import Lib (runSearch')

test = hspec spec

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
      , TimeStamp {hr = 8, min = 39, sec = 28}
      , Read "Catch-22" "Joseph Heller")
  , TabTsEntry
      (1, TimeStamp {hr = 8, min = 42, sec = 14}, Def (Defn Nothing ["acedia"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 8, min = 46, sec = 48}
      , Def (Defn Nothing ["jaundice"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 8, min = 50, sec = 22}
      , Def (Defn Nothing ["matriot", "patriot"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 1, sec = 19}
      , Def (Defn Nothing ["infundibuliform", "vestibular", "hyomandibular"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 3, sec = 58}
      , Def (Defn Nothing ["putrescent"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 7, sec = 41}
      , Def
          (DefVersus
             "supra-"
             "over, or above, but not touching; transcending."
             "super-"
             "exceeding; above; inclusive; on top; superior in size, degree, position, etc."))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 22, sec = 17}
      , Def (Defn Nothing ["jubilee"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 31, sec = 11}
      , Def (Defn Nothing ["bituminous"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 37, sec = 43}
      , Quotation
          "He had decided to live forever or die in the attempt..."
          "In \"Catch-22\" by Joseph Heller"
          Nothing)
  , TabTsEntry
      (1, TimeStamp {hr = 9, min = 38, sec = 6}, Def (Defn Nothing ["flak"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 41, sec = 51}
      , Def (Defn Nothing ["saturnine"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 55, sec = 16}
      , Def
          (Defn Nothing ["gentian", "elephantiasis", "homiletic", "hortatory"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 16, min = 28, sec = 17}
      , Def (Defn Nothing ["hagiography"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 16, min = 40, sec = 45}
      , Def (Defn Nothing ["traint"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 16, min = 45, sec = 39}
      , Def (Defn Nothing ["stertorous", "oner", "noy"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 16, min = 59, sec = 22}
      , Quotation
          "We compensate for our anxiety, insignificance, with egotistic behavior--e.g., by obsessing over the reception of our contributions-- with self-consciousness."
          "Keane Yahn-Krafft"
          Nothing)
  , TabTsEntry (0, TimeStamp {hr = 17, min = 0, sec = 14}, Read "test" "Author")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 0, sec = 33}
      , Def (Defn Nothing ["fop", "aide", "ey", "lapis", "vamp"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 10, sec = 53}
      , Def (Defn Nothing ["quaints"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 17, sec = 45}
      , Def (Defn Nothing ["dado", "emu", "aieee"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 20, sec = 52}
      , Def (Defn Nothing ["drinkle"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 22, sec = 57}
      , Def (Defn Nothing ["gar", "qa", "cit", "ragu", "wen", "sadist", "id"]))
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
spec
  -- Wipes test DB before each test. write another context setup/teardown
  -- function to persist (but don't; tests results ought to be replicable
  -- whenever possible)
 = do
  around (withNewConnFrom "testState/DB") $ do
    describe "Add day's worth of logs to DB." $
      it
        "converts and inserts logs to DB; compares with conversion directly to `Results`" $ \acid -> do
        utc <- getCurrentTime
        let backupDay =  utctDay . incrMin $ utc
            day = maybe backupDay id $ pathToDay "18.11.20"
        update acid $ AddDay day utc breakage
        let r' = Results $ fromLogEntry day <$> breakage
        r <- query acid FromDB
        last <- query acid ViewDB >>= return . \DB {lastUpdated} -> fromJust $ getOne lastUpdated
        utc `shouldBe` modified last
        r `shouldBe` r'
        return ()
    describe "Dump filtration." $
      it "Fetches dumps that contain the string \"dump\"." $ \acid -> do
        day <- utctDay <$> getCurrentTime
        update acid $ InsertDump day "wasss"
        update acid $ InsertDump day "hello dump"
        update acid $ InsertDump day "goodbye dump"
        let s = addDays (-182) day -- ~ six months
            e = day
            search =
              Search s e [] $
              BucketList [T.isInfixOf "dump"] ([], []) [] [] [] ([], []) []
        query acid ViewDB >>= \DB {dumped} ->
          filterDumps search dumped `shouldBe`
          [DumpR "goodbye dump", DumpR "hello dump"]
    describe "Comment filtration." $
      it "applies single comment search pred" $ \acid -> do
        day <- getCurrentTime
        day' <- incrMin <$> getCurrentTime
        day'' <- incrMin . incrMin <$> getCurrentTime
        update acid $ UpdateComment day "comment body 1" Nothing
        update acid $ UpdateComment day' "synthesis" Nothing
        update acid $ UpdateComment day'' "comment body 2" Nothing
        let s = addDays (-182) $ utctDay day -- ~ six months
            e = utctDay day
            search =
              Search s e [] $
              BucketList [] ([], []) [] [] [] ([], []) [T.isInfixOf "comment"]
        query acid ViewDB >>= \db@DB {comments} -> do
          filterComments db search comments `shouldBe`
            [ TsR (truncateUTC day) (Commentary "comment body 1")
            , TsR (truncateUTC day'') (Commentary "comment body 2")
            ]
    tDia
    tQuote
    tDefs
    tPhrases

--        query acid FromDB>>=colRender True
-- TODO: 
--
-- # First Pass
--
-- test (non-exhaustively) a few preds on a few entries of the given
-- type; don't cover:
--
--  * all pred combos for a given entry variant
--  * chrono filtration, that is, bucket filter dispatch
--
-- ▣  comments
-- ▣  dumped
-- ▣  dialogues
-- ▣  quotes
-- ▣  defs
-- ▣  phrases
-- □  (??) reads, perhaps only in chrono?
--
-- # Second pass
--
-- cover:
-- * bucket dispatch/chrono 
-- * more exhaustive per variant predicate combos
--
-- □  chronological filters (port `filterWith'` to use 'chrono')
--
--    perhaps enable w/ `--chrono/list-mode`?
--
-- □  single predicate bucket dispatch (no list mode.
--
--    CLI defualt option: group output by type, since most query invocations will only
--    focus on a single entry variant. accordingly, entry variant specific query options 
--    have been expanded significantly. 
--
tDia =
  describe "dialogues: 4" $
  it "dialogue single single searh pred; no preds returns all in date range" $ \acid -> do
    day <- truncateUTC <$> getCurrentTime
    day' <- truncateUTC . incrMin <$> getCurrentTime
    day'' <- truncateUTC . incrMin . incrMin <$> getCurrentTime
    let d =
          [r|
    MARGO: (rushes forward) I love my mom—she's perfect!
    NICOLE: (steps back embracingly; brushes away and downwards.) No. She's my lawyer.
    |]
    let d' =
          [r| 
    (After ~1hr of unbridled loquacity, having mercifully dammed mom's torrent)
    MOM: Do you mind me telling all my favorite moments?
    (Without looking up from his guitar playing)
    DAD: No, just get it over with.
    |]
    update acid $ UpdateDialogue day' d
    update acid $ UpdateDialogue day'' d'
    let s = addDays (-182) $ utctDay day -- ~ six months
        e = utctDay day
        mkSearch = Search s e []
        search =
          mkSearch initBucketList {dialoguesPreds = [T.isInfixOf "MARGO"]}
        search' = mkSearch initBucketList {dialoguesPreds = [T.isInfixOf "mom"]}
        search'' = mkSearch initBucketList
    query acid ViewDB >>= \db@DB {dialogues}
      --print $ T.isInfixOf "mom" d
      --pPrint dialogues
     -> do
      let first = TsR day' . Dialogue $ T.unpack d
          second = TsR day'' . Dialogue $ T.unpack d'
      filterDialogues db search dialogues `shouldBe` [first]
      filterDialogues db search' dialogues `shouldBe` [first, second]
      filterDialogues db search'' dialogues `shouldBe` [first, second]

tQuote =
  describe "quotes: 6" $
  it "applies single quote search pred" $ \acid -> do
    day <- getCurrentTime
    day' <- incrMin <$> getCurrentTime
    day'' <- incrMin . incrMin <$> getCurrentTime
    let q =
          "... nothing gave him more faith in the afterlife than the inexorable sarcasm of Fate."
        q' =
          "Above all, he wanted to stop being a child without using the cheap disguise of becoming a parent."
        q'' =
          "He who could call a spade a spade should be compelled to use one."
        q''' = "Three people said it. It must be true."
        edward = "Edward St. Aubyn"
        oscar = "Oscar Wilde"
    update acid $ UpdateRead day "The Picture of Dorian Gray" "Oscar Wilde"
    update acid $
      UpdateQuote
        day'
        q''
        oscar
        Nothing -- att attrTag
        -- FIXME truncate utctimes on entry to DB
        (Just . AttrTag $ truncateUTC day)
    update acid $
      UpdateQuote
        day''
        q
        edward
        Nothing -- att attrTag
        Nothing
    update acid $
      UpdateQuote
        (incrMin day'')
        q'
        edward
        Nothing -- att attrTag
        Nothing
    update acid $
      UpdateQuote
        (incrMin $ incrMin day'')
        q'''
        oscar
        Nothing
        (Just . AttrTag $ truncateUTC day)
    let s = addDays (-182) $ utctDay day -- ~ six months
        e = utctDay . incrMin . incrMin . incrMin $ day''
        byWilde (Attribution _ a) =
          T.isInfixOf "Oscar" a && T.isInfixOf "Wilde" a
        search =
          Search s e [] initBucketList {quotesPreds = [T.isInfixOf "sarcasm"]}
        search' =
          Search s e [] initBucketList {quotesPreds = [T.isInfixOf "spade"]}
        search'' =
          Search
            s
            e
            []
            initBucketList
            {quotesPreds = [T.isInfixOf "child", T.isInfixOf "cheap disguise"]}
        search''' = Search s e [byWilde] initBucketList {quotesPreds = []}
        search'''' =
          Search
            s
            e
            [byWilde]
            initBucketList {quotesPreds = [T.isInfixOf "spade"]}
    query acid ViewDB >>= \db@DB {quotes, reads} -> do
      let tag =
            (mkTsIdxTag
               day'
               (q'', oscar, Nothing)
               (Just . AttrTag . truncateUTC $ day))
          o1 = TsR (truncateUTC day') $ Quotation (T.unpack q'') (T.unpack oscar) Nothing
          o2 = TsR (truncateUTC (incrMin . incrMin $ day'')) 
              $ Quotation (T.unpack q''') (T.unpack oscar) Nothing
          e1 = TsR (truncateUTC day'') 
              $ Quotation (T.unpack q) (T.unpack edward) Nothing
          e2 = TsR (truncateUTC $ incrMin day'') 
              $ Quotation (T.unpack q') (T.unpack edward) Nothing

      -- empty
      filterQuotes db (Search s e [] initBucketList) quotes `shouldBe`
        [ o1 , e1 , e2 , o2 ]
      -- spade
      filterQuotes db search' quotes `shouldBe` [o1]
      -- sarcasm 
      filterQuotes db search quotes `shouldBe` [e1]
      -- cheap disguise & child
      filterQuotes db search'' quotes `shouldBe` [e2]
      -- auth pred: Oscar Wilde
      filterQuotes db search''' quotes `shouldBe`
        [o1, o2]
      -- auth pred (Oscar Wilde) & contains "spade"
      filterQuotes db search'''' quotes `shouldBe` [o1]

tDefs =
  describe "definitions: 6" $
  it "definitions: headword, and meaning predicates" $ \acid -> do
    day <- truncateUTC <$> getCurrentTime
    day' <- truncateUTC . incrMin <$> getCurrentTime
    day'' <- truncateUTC . incrMin . incrMin <$> getCurrentTime
    day''' <- truncateUTC . incrMin . incrMin . incrMin <$> getCurrentTime
    day'''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin <$> getCurrentTime
    day''''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    day'''''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    let envy = Defn Nothing ["emulous", "invidious", "enviable"]
        serry = InlineDef "serry" "to crowd; to press together"
        jostle = InlineDef "jostle" "to crowd; to run against; to bump"
        jonquil = InlineDef "jonquil" "a yellow flower; a shade of yellow"
        jonquil' = InlineDef "jonquil plus" "a yellow flower; a shade of yellow"
        disen =
          DefVersus
            "dissent"
            "verbal or civil contrariety"
            "dissension"
            "a more violent subversion, or opposition to"
    update acid $ UpdateDef day' envy Nothing
    update acid $ UpdateDef day'' serry Nothing
    update acid $ UpdateDef day''' jonquil Nothing
    update acid $ UpdateDef day'''' disen Nothing
    update acid $ UpdateDef day''''' jostle Nothing
    update acid $ UpdateDef day'''''' jonquil' Nothing
    let s = addDays (-182) $ utctDay day -- ~ six months
        e = utctDay (incrMin day'''''')
        search = Search s e [] initBucketList {defsPreds = ([], [])}
        search' =
          Search s e [] initBucketList {defsPreds = ([], [isInfixOf "crowd"])}
        search'' = Search s e [] initBucketList
        search''' =
          Search s e [] initBucketList {defsPreds = ([isInfixOf "jonquil"], [])}
        search'''' =
          Search
            s
            e
            []
            initBucketList {defsPreds = ([isInfixOf "e"], [isInfixOf "crowd"])}
        searchVs =
          Search s e [] initBucketList {defsPreds = ([isInfixOf "dissent"], [])}
        searchVs' =
          Search
            s
            e
            []
            initBucketList {defsPreds = ([], [isInfixOf "civil contrariety"])}
        searchVs'' =
          Search
            s
            e
            []
            initBucketList
            { defsPreds =
                ([isInfixOf "dissent"], [isInfixOf "civil contrariety"])
            }
    query acid ViewDB >>= \db@DB {defs} -> do
      let envy' = TsR day' (Def envy)
          serry' = TsR day'' (Def serry)
          jonquil'' = TsR day''' (Def jonquil)
          disen' = TsR day'''' (Def disen)
          jostle' = TsR day''''' (Def jostle)
          jonquil''' = TsR day'''''' (Def jonquil')
      filterDefs db search defs `shouldBe`
        [ TsR day' (Def envy)
        , TsR day'' (Def serry)
        , TsR day''' (Def jonquil)
        , TsR day'''' (Def disen)
        , TsR day''''' (Def jostle)
        , TsR day'''''' (Def jonquil')
        ]
      -- no headword preds
      filterDefs db search' defs `shouldBe` [serry', jostle']
      -- no meaning preds
      filterDefs db search''' defs `shouldBe` [jonquil'', jonquil''']
      -- both
      filterDefs db search'''' defs `shouldBe` [serry', jostle']
      -- versus no meaning preds
      filterDefs db searchVs defs `shouldBe` [disen']
      -- versus no headword preds
      filterDefs db searchVs' defs `shouldBe` [disen']
      -- both
      filterDefs db searchVs'' defs `shouldBe` [disen']

tPhrases =
  describe "definitions: 6" $
  it "definitions: headword, and meaning predicates" $ \acid -> do
    day <- truncateUTC <$> getCurrentTime
    day' <- truncateUTC . incrMin <$> getCurrentTime
    day'' <- truncateUTC . incrMin . incrMin <$> getCurrentTime
    day''' <- truncateUTC . incrMin . incrMin . incrMin <$> getCurrentTime
    day'''' <- truncateUTC . incrMin . incrMin . incrMin . incrMin <$> getCurrentTime
    day''''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin . incrMin <$> getCurrentTime
    day'''''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    day''''''' <-
      truncateUTC . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    day'''''''' <-
      truncateUTC . incrMin .
      incrMin . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    day''''''''' <-
      truncateUTC . incrMin .
      incrMin .
      incrMin . incrMin . incrMin . incrMin . incrMin . incrMin . incrMin <$>
      getCurrentTime
    let ever = Plural ["ever and anon"]
        tacit = Plural ["supercilious taciturnity"]
        arbiter =
          Defined
            "ARBITER ELEGANTIARUM"
            "a judge of artistic taste and etiquette."
        amour = Defined "AMOUR PROPRE" "self-love, -esteem"
        byWay =
          Defined "WAY OF BEING" "via; in the condition of position of being"
        sine =
          Defined
            "BY SINE QUA NON"
            "an essential condition; something necessary"
        irony = Plural ["prophylactic irony"]
        mezzo =
          Defined
            "BY MEZZO DEL CAMMIN DI NOSTRA VITA"
            "\"When I had journeyed half of our life's way\" -- \"Dante's Inferno\""
        trompe =
          Defined "TROMPE L'ŒIL" "of art having an illusory third dimension."
    update acid $ UpdatePhrase day' ever Nothing
    update acid $ UpdatePhrase day'' tacit Nothing
    update acid $ UpdatePhrase day''' arbiter Nothing
    update acid $ UpdatePhrase day'''' amour Nothing
    update acid $ UpdatePhrase day''''' byWay Nothing
    update acid $ UpdatePhrase day'''''' sine Nothing
    update acid $ UpdatePhrase day''''''' irony Nothing
    update acid $ UpdatePhrase day'''''''' mezzo Nothing
    update acid $ UpdatePhrase day''''''''' trompe Nothing
    let s = addDays (-182) $ utctDay day -- ~ six months
        e = utctDay (incrMin day'''''')
        search = Search s e [] initBucketList
    query acid ViewDB >>= \db@DB {phrases} -> do
      --runSearch' True True (Search s e [] initBucketList {phrasesPreds = ([isInfixOf "TROMPE"],[])}) db
      let ever' = TsR day' $ Phr ever
          tacit' = TsR day'' $ Phr tacit
          arbiter' = TsR day''' $ Phr arbiter
          amour' = TsR day'''' $ Phr amour
          byWay' = TsR day''''' $ Phr byWay
          sine' = TsR  day'''''' $ Phr sine
          irony' = TsR day''''''' $ Phr irony
          mezzo' = TsR day'''''''' $ Phr mezzo
          trompe' = TsR day''''''''' $ Phr trompe
      filterPhrases db search phrases `shouldBe`
        [ ever'
        , tacit'
        , arbiter'
        , amour'
        , byWay'
        , sine'
        , irony'
        , mezzo'
        , trompe'
        ]
      -- no headword preds
      filterPhrases
        db
        (Search
           s
           e
           []
           initBucketList {phrasesPreds = ([], [isInfixOf "condition"])})
        phrases `shouldBe`
        [byWay', sine']
      -- no meaning preds
      filterPhrases
        db
        (Search s e [] initBucketList {phrasesPreds = ([isInfixOf "BY"], [])})
        phrases `shouldBe`
        [sine', mezzo']
      -- both
      filterPhrases
        db
        (Search
           s
           e
           []
           initBucketList
           {phrasesPreds = ([isInfixOf "TROMPE"], [isInfixOf "illusory third"])})
        phrases `shouldBe`
        [trompe']

data EntryType
  = Checkpoint
  | Event

-- | Fetches 'LogKey' of requested log type at the given acid-state path.
logKey :: EntryType -> FilePath -> LogKey object
logKey et fp =
  case et of
    Checkpoint -> LogKey fp "checkpoints"
    Event -> LogKey fp "events"

rollbackExample = do
  let k = logKey Checkpoint "state/DB" :: LogKey Checkpoint
  fl <- openFileLog k
  id <- askCurrentEntryId fl
  let prev = id - 2
  print $ "rolling back from " <> show id <> " to " <> show prev
  closeFileLog fl
  --rollbackTo k prev
  --

dbTest = hspec . around (withNewConnFrom "testState/DB")

scratch = do
  day <- getCurrentTime
  day' <- incrMin <$> getCurrentTime
  day'' <- incrMin . incrMin <$> getCurrentTime
  bracket
    (openLocalStateFrom "state/DB" initDB)
    createCheckpointAndClose
    (\acid
      --update acid $ InsertDump day "sayonara dump"
      -> do
       update acid ReinitDB
       
       let s = addDays (-182) $ utctDay day -- ~ six months
           e = utctDay day
           search =
             Search s e [] $
             BucketList [] ([], []) [] [] [] ([], []) [T.isInfixOf "comment"]
       update acid $ AddDay (utctDay day) (truncateUTC day) read2
       update acid $ AddDay (incrDay $ utctDay day) (truncateUTC $ incrUTC day) read1
       query acid LastRead >>= colRender True
       --query acid FromDB >>= colRender True
       --query acid ViewDB >>= \db -> pPrint db
    )
  return ()

read2 = 
  [ TabTsEntry
      ( 0
      , TimeStamp { hr = 12 , min = 38 , sec = 51 }
      , Read "Sense and Sensibility" "Jane Austen"
      )

  ]

read1 =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 38, sec = 51}
      , Read "Pride and Prejudice" "Jane Austen")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 39, sec = 5}
      , Def
          (Defn
             Nothing
             [ "ductility"
             , "stricture"
             , "negative"
             , "archness"
             , "arch"
             , "celerity"
             ]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 17, min = 54, sec = 8}
      , Read "Wuthering Heights" "Emily Bronte")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 17, min = 53, sec = 21}
      , Read "Catch 22" "Joseph Heller")
  ]
