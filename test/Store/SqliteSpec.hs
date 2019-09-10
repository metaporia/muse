{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, NamedFieldPuns,
  OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store.SqliteSpec
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Tests Sqlite data store.
-----------------------------------------------------------------------------
module Store.SqliteSpec where



import           Control.Exception              ( bracket )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Bifunctor                 ( bimap )
import           Data.Foldable                  ( traverse_ )
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime(..)
                                                , addDays
                                                )
import           Data.Time.Calendar             ( Day )
import           Data.Time.Clock                ( getCurrentTime )
import qualified Database.Esqueleto            as E
import           Database.Esqueleto             ( (^.)
                                                , from
                                                , like
                                                , where_
                                                )
import           Database.Persist
import           Database.Persist.Sqlite
import           Debug.Trace
import           Helpers
import           Parse
import qualified Parse                         as P
import           Parse.Entry
import qualified Parse.Entry                   as P
import           Store.Sqlite
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )
import           Time

asIO :: IO a -> IO a
asIO a = a

test :: IO ()
test = hspec spec

spec :: Spec
spec = do
  let setup writeAction = do
        runMigrationSilent migrateAll
        today <- liftIO $ utctDay <$> getCurrentTime
        writeAction today
        before <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
        since  <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime
        return (since, before)
      filterQuotesSetup = setup (`writeDay` demoLogEntries)
      defVarSetup = setup (\today -> do
        let entries = fromJust $ toMaybe $ parse logEntries defVar
        writeDay today entries)

  describe "quote filtration" $ do
    it
        "manually attributed nested quotes are filtered as though they have two attributions (see Woolf in Eliot)"
      $ asIO
      $ runSqlInMem -- since it runs in mem, no pre/post clear is necessary
      $ do
          filterQuotesSetup
          satisfactoryDefs <-
            selectList ([] :: [Filter QuoteEntry]) []
              >>= applyReadPreds ["Woolf"] ["Light"]
          liftIO $ length satisfactoryDefs `shouldBe` 3
    it "filterQuotes" $ asIO $ runSqlInMem $ do
      (since, before) <- filterQuotesSetup
      matchingQuotes  <- filterQuotes since
                                      before
                                      ["%Woolf%", "%Virginia%"]
                                      []
                                      ["%simplicity%"]
      liftIO
        $          quoteEntryBody
        .          entityVal
        <$>        matchingQuotes
        `shouldBe` ["Her simplicity fathomed what clever people falsified."]
      noMatches <- filterQuotes since
                                before
                                ["%Woolf%", "%Thomas Moore%"]
                                []
                                ["%simplicity%"]
      liftIO $ quoteEntryBody . entityVal <$> noMatches `shouldBe` []
  describe "definition filtration" $ do 
    it "all def varians" $ asIO $ runSqlInMem $ do
      (since, before) <- filterQuotesSetup
      defs            <- filterDefs
        "/home/aporia/.muse/"
        since
        before
        []
        []
        (DefSearch [Defn', Phrase', InlineDef', DefVersus'] [] [])
      liftIO $ defs `shouldBe` demoDefsAll
    it "def versus variants" $ asIO $ runSqlInMem $ do
      (since, before) <- filterQuotesSetup
      defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [DefVersus'] [] [])
      liftIO $ defs `shouldBe` demoDefVersus
    it "vanilla definition variants" $ asIO $ runSqlInMem $ do
      (since, before) <- filterQuotesSetup
      defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [Defn'] [] [])
      liftIO $ defs `shouldBe` demoDefn
    it "inline def definition variants" $ asIO $ runSqlInMem $ do
      (since, before) <- defVarSetup
      defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [InlineDef'] [] [])
      liftIO $ defs `shouldBe` varInline
    it "phrase definition variants" $ asIO $ runSqlInMem $ do
       (since, before) <- defVarSetup
       defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [Phrase'] [] [])
       liftIO $ defs `shouldBe` varPhrase
     



runSqlInMem = runSqliteInfo $ mkSqliteConnectionInfo ":memory:"

run :: IO ()
run = runSqlInMem $ do
  runMigration migrateAll
  let entries = fromJust $ toMaybe $ parse logEntries defVar
  today <- liftIO $ utctDay <$> getCurrentTime
  writeDay today entries

  before         <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since          <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime

  defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [Phrase'] [] [])
  liftIO $ pPrint defs

  return ()


demo :: IO ()
demo = runSqlite "sqliteSpec.db" $ do
  runMigration migrateAll
  today <- liftIO $ utctDay <$> getCurrentTime
  -- write 'demoLogEntries'
  -- sequence_ $ fmap (\logEntry -> writeLogEntry today logEntry Nothing) demoLogEntries
  -- revision: tagging w/ 'writeDay'
  es    <- writeDay today demoLogEntries
  liftIO $ sequence $ either putStrLn print <$> es
  -- select all by author 

  -- test attribution of 'QuoteEntry'
  -- 1. get quoteEntry
  qe :: Entity QuoteEntry <- head <$> selectList ([] :: [Filter QuoteEntry]) []
  satisfies               <- taggedEntrySatisfies [] [] qe
  let readKey = fromJust $ do
        entity <- satisfies
        getAttributionTag $ entityVal entity

  --liftIO $ pPrint readKey
  --liftIO
  --  $  putStrLn
  --  $  "satisfies: "
  --  <> show (isJust satisfies)
  --  <> "\nentry: "
  --  <> ppShow satisfies
  readEntry        <- get readKey
  --liftIO $ putStrLn  $ "readEntry: " <> ppShow readEntry
  attrSatisfies    <- attributionSatisfies ["Woolf"] [] readKey
  --liftIO $ putStrLn $ "attrSatisfies: " <> show attrSatisfies
  satisfactoryDefs <-
    selectList ([] :: [Filter QuoteEntry]) []
      >>= applyReadPreds ["Woolf"] ["Light"]
  before         <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since          <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime
  matchingQuotes <- filterQuotes since
                                 before
                                 ["%Woolf%"]
                                 ["%Lighthouse%"]
                                 ["%simplicity%"]
  -- liftIO $ pPrint $ (quoteEntryBody . entityVal) <$> matchingQuotes -- -- satisfactoryDefs
  --let newRead = ReadEntry {readEntryTitle = "Title", readEntryAuthor = "Author"} liftIO $ putStrLn "before repsert"
  --selectList ([] :: [Filter ReadEntry]) [] >>= liftIO . traverse_
  --  (pPrint . entityVal)
  --repsert readKey newRead
  --liftIO $ putStrLn "after repsert"
  --selectList ([] :: [Filter ReadEntry]) [] >>= liftIO . traverse_
  --  (pPrint . entityVal)
  defs <- filterDefs "/home/aporia/.muse/" since before [] [] (DefSearch [InlineDef'] [] [])
  liftIO $ pPrint defs

  return ()

clear :: IO ()
clear = runSqlite "sqliteSpec.db" $ do
  runMigration migrateAll
  clear' 

clear' :: MonadIO m => DB m ()
clear' = do
  deleteWhere ([] :: [Filter DefEntry])
  deleteWhere ([] :: [Filter ReadEntry])
  deleteWhere ([] :: [Filter QuoteEntry])
  deleteWhere ([] :: [Filter CommentaryEntry])
  deleteWhere ([] :: [Filter DialogueEntry])
  deleteWhere ([] :: [Filter PageNumberEntry])


demoLogEntries :: [LogEntry]
demoLogEntries
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf"
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 55, sec = 17}
      , Def
        (DefVersus "benignant"
                   "kind; gracious; favorable;"
                   "benign"
                   "gentle, mild, or, medically, non-threatening"
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 45}
      , Def
        (DefVersus
          "malignant"
          "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
          "malign"
          "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Def (Defn (Just 38) ["inimical", "traduce", "virulent"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 23, sec = 0}
      , Read "Silas Marner" "George Eliot (Mary Ann Evans)"
      )
      -- N.B. this read block is here to ensure that manually attributed (indented)
      -- entries are treated by search predicates as belonging to both.
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
        "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
        "In \"To the Lighthouse\", by Virginia Woolf"
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation "Her simplicity fathomed what clever people falsified."
                  "In \"To the Lighthouse\", by Virginia Woolf"
                  Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf"
        (Just 38)
      )
    ]


curr :: IO ()
curr = runSqlite "test1.db" $ do
  runMigration migrateAll
  today <- liftIO $ utctDay <$> getCurrentTime
  let -- logEntries :: _
      logEntries' = statefulValidatedMany Nothing
                                          logEntrySquawk
                                          logEntryPassNewestTimeStamp
  liftIO $ pPrint $ parse logEntries' reallyBroken
  --writeDay today $ fromJust $ toMaybe $ parse logEntries reallyBroken
  return ()

reallyBroken = [r|
09:20:55 λ. read "Dead Souls", by Gogol
    09:21:02 λ. s32
    09:27:13 λ. e34
09:27:16 λ. read "Infinite Jest", by David Foster Wallace
    09:27:25 λ. s87
    09:36:38 λ. d89 chapeau
    09:46:04 λ. d91 specular
    09:48:02 λ. d91 pendentive
    09:49:37 λ. d91 auteur
    09:55:19 λ. d92 sobriquet
    09:58:17 λ. d92 fulvous: dull yellow w/ mixture of gray/brown.
    09:59:45 λ. d92 teratogenic
    10:13:32 λ. d eponymous, eponym
    10:16:47 λ. d97 quiescent
    10:17:45 λ. d 97 digitate
    10:21:39 λ. d98 ephebe: a (ancient Gr.) young man, 18-20, in military training.
    10:28:20 λ. d101 semion
    10:29:51 λ. d101 zygomatic
    10:41:00 λ. e105
    10:41:00 λ. s105
    10:50:05 λ. d coruscate
    10:52:57 λ. d105 torpid
    11:02:52 λ. d108 lume
    11:11:30 λ. df304 cant, escutcheon
    11:25:53 λ. df308 interdict
    11:28:09 λ. df038 solipsism
    11:29:21 λ. df308 ken
    11:37:36 λ. df308 episcopate
    11:40:20 λ. df308 flange, numinous, vulgate
    11:50:48 λ. df308 rostrum
    11:54:24 λ. d109 gibbous
    11:54:51 λ. e109
    12:05:24 λ. s109
    12:11:27 λ. d111 adenoid
    13:15:12 λ. e135
13:48:10 λ. read "Dead Souls", by Gogol
    13:48:18 λ. s34
    14:15:38 λ. e45
    20:38:14 λ. s87
    20:38:37 λ. d adze
    20:38:55 λ. d kaftan
    20:39:13 λ. d empyrean
    20:39:35 λ. d lathe, faience, hummock, vittles, victuals, aureate, gilt
    20:43:05 λ. d veriest, circumspect
|]

sample = [r|
13:48:10 λ. read "Dead Souls", by Gogol
    13:48:18 λ. s34
    14:15:38 λ. e45
    20:38:14 λ. s87
    20:38:37 λ. d adze
    20:38:55 λ. d kaftan
    20:39:13 λ. d empyrean
    20:39:35 λ. d lathe, faience, hummock, vittles, victuals, aureate, gilt
    20:43:05 λ. d veriest, circumspect
|]

defVar = [r|
07:52:16 λ. read "Northanger Abbey" by Jane Austen
    10:38:00 λ. d whorl : (to form) a pattern of concentric circles
    11:46:47 λ. d philippic
    13:22:53 λ. phrase "fever of suspense"
    13:52:17 λ. d mizzle: a misty drizzle
    14:51:13 λ. d gaucherie, gauche
    16:02:03 λ. phr "smarting under an obligation"
    16:40:05 λ. d shillelagh: an Irish, knob-ended cudgel
    17:02:16 λ. d marge
    18:38:48 λ. d saccadic: of jerky, discontinous movement
|]

demoDefsAll
  = [ Def
      (DefVersus "benignant"
                 "kind; gracious; favorable;"
                 "benign"
                 "gentle, mild, or, medically, non-threatening"
      )
    , Def
      (DefVersus
        "malignant"
        "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
        "malign"
        "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."
      )
    , Def (Defn Nothing ["inimical", "traduce", "virulent"])
    , Def (Defn Nothing ["sublime", "lintel"])
    , Def (Defn Nothing ["plover"])
    , Def (Defn Nothing ["cosmogony"])
    ]

demoDefVersus
  = [ Def
      (DefVersus "benignant"
                 "kind; gracious; favorable;"
                 "benign"
                 "gentle, mild, or, medically, non-threatening"
      )
    , Def
      (DefVersus
        "malignant"
        "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
        "malign"
        "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."
      )
    ]

demoDefn =
  [ Def (Defn Nothing ["inimical", "traduce", "virulent"])
  , Def (Defn Nothing ["sublime", "lintel"])
  , Def (Defn Nothing ["plover"])
  , Def (Defn Nothing ["cosmogony"])
  ]

varInline =
  [ Def (P.InlineDef "whorl" "(to form) a pattern of concentric circles")
  , Def (P.InlineDef "mizzle" "a misty drizzle")
  , Def (P.InlineDef "shillelagh" "an Irish, knob-ended cudgel")
  , Def (P.InlineDef "saccadic" "of jerky, discontinous movement")
  ]

varPhrase =
  [ Phr (Plural ["\"fever of suspense\""])
  , Phr (Plural ["\"smarting under an obligation\""])
  ]
