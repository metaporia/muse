{-# LANGUAGE QuasiQuotes, NamedFieldPuns, OverloadedStrings,
  RecordWildCards #-}

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

import Database.Persist
import Database.Persist.Sqlite
import Test.Hspec
import Store.Sqlite
import Time
import Data.Time (UTCTime(..))
import Data.Time.Clock (getCurrentTime)
import Parse.Entry
import Parse
import Control.Monad.IO.Class (liftIO)


spec :: Spec
spec = 
  describe "stub" $ do
    it "satisfies hspec test runner" $
      1 `shouldBe` 1

demo :: IO ()
demo = 
  runSqlite "sqliteSpec.db" $ do
    runMigration migrateAll
    today <- liftIO $ utctDay <$> getCurrentTime
    -- write 'demoLogEntries'
    -- sequence_ $ fmap (\logEntry -> writeLogEntry today logEntry Nothing) demoLogEntries
    -- revision: tagging w/ 'writeDay'
    es <- writeDay today demoLogEntries
    liftIO $ sequence $ either putStrLn print <$> es

    --clear

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


demoLogEntries :: [LogEntry]
demoLogEntries =
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
      ( 0
      , TimeStamp {hr = 10, min = 23, sec = 0}
      , Read "To the Lighthouse" "Virginia Woolf")

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


