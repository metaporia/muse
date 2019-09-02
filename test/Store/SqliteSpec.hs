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

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Exception              ( bracket )
import           Data.List                      ( isInfixOf )
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                , isNothing
                                                , fromJust
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime(..) )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Calendar             ( Day )
import           Database.Persist
import           Database.Persist.Sqlite
import           Debug.Trace
import           Helpers
import           Parse
import           Parse.Entry
import qualified Parse.Entry as P
import           Data.Semigroup                 ( (<>) )
import           Store.Sqlite
import           Test.Hspec
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )
import           Time
import qualified Parse as P


asIO :: IO a -> IO a
asIO a = a

test :: IO ()
test = hspec spec

spec :: Spec
spec = do
  describe "quote filtration" $ do
    it
        "manually attributed nested quotes are filtered as though they have two attributions (see Woolf in Eliot)"
      $ asIO
      $ runSqlInMem -- since it runs in mem, no pre/post clear is necessary
      $ do
          cmdList <- runMigrationSilent migrateAll
          today            <- liftIO $ utctDay <$> getCurrentTime
          es               <- writeDay today demoLogEntries
          satisfactoryDefs <-
            selectList ([] :: [Filter QuoteEntry]) []
              >>= applyReadPreds ["Woolf"] ["Light"]
          liftIO $ length satisfactoryDefs `shouldBe` 3
    it "TODO"
      $ asIO
      $ runSqlInMem
      $ do
          cmdList <- runMigrationSilent migrateAll
          today            <- liftIO $ utctDay <$> getCurrentTime
          writeDay today demoLogEntries
          qs <- selectList ([] :: [Filter QuoteEntry]) [] >>= applyReadPreds ["Woolf"] ["Light"] 
          liftIO (length qs `shouldBe` 3)


runSqlInMem = runSqliteInfo $ mkSqliteConnectionInfo ":memory:"

--dbTest :: Spec
--dbTest = do
--  before clear $ do
--    describe "demo" $ do
--      it "it" $ (do runSqlite "sqliteSpec.db" $ do return 1 ) `shouldReturn` 1

--runWithDB :: MonadIO m => T.Text -> DB m a -> IO a
--runWithDB db action = runSqlite db $ do
--  runMigration migrateAll
--  today <- liftIO $ utctDay <$> getCurrentTime
--  es    <- writeDay today demoLogEntries
--  action 


-- infix
quoteSearch:: MonadIO m => T.Text -> DB m [Entity QuoteEntry]
quoteSearch searchTerm = rawSql
  (  "select ?? from quote_entry where manual_attribution like '%"
  <> searchTerm
  <> "%'"
  )
  []

-- infix
quoteSearch2 :: MonadIO m => T.Text -> T.Text -> DB m [Entity QuoteEntry]
quoteSearch2 attr body = rawSql
  (  "select ?? from quote_entry where manual_attribution like '%"
  <> attr
  <> "%' and body like '%" <> body <> "%'"
  )
  []



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
  readEntry <- get readKey
  --liftIO $ putStrLn  $ "readEntry: " <> ppShow readEntry
  attrSatisfies <- attributionSatisfies ["Woolf"] [] readKey 
  --liftIO $ putStrLn $ "attrSatisfies: " <> show attrSatisfies
  satisfactoryDefs <- selectList ([] :: [Filter QuoteEntry]) [] >>= applyReadPreds ["Woolf"] ["Light"]
  matchingQuotes <- quoteSearch2 "Woolf" "simplicity"
  liftIO $ traverse pPrint matchingQuotes -- -- satisfactoryDefs
  return ()

clear :: IO ()
clear = runSqlite "sqliteSpec.db" $ do
  runMigration migrateAll
  deleteWhere ([] :: [Filter DefEntry])
  deleteWhere ([] :: [Filter ReadEntry])
  deleteWhere ([] :: [Filter QuoteEntry])
  deleteWhere ([] :: [Filter CommentaryEntry])
  deleteWhere ([] :: [Filter DialogueEntry])
  deleteWhere ([] :: [Filter PageNumberEntry])

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
