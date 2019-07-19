{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, NamedFieldPuns,
  OverloadedStrings, RecordWildCards #-}

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

import Text.Show.Pretty (pPrint, ppShow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Time (UTCTime(..))
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Parse
import Parse.Entry
import Store.Sqlite
import Test.Hspec
import Time

spec :: Spec
spec = describe "stub" $ do it "satisfies hspec test runner" $ 1 `shouldBe` 1

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
    -- select all by author 
    let attributionSatisfies ::
             MonadIO m
          => Maybe String
          -> Maybe String
          -> Key ReadEntry
          -> DB m (Maybe Bool)
        attributionSatisfies authorInfix titleInfix readKey = do
          maybeReadEntry <- get readKey
          return $ do
            let andWhenBothPresent (Just p) (Just q) = p && q
                andWhenBothPresent (Just p) Nothing = p
                andWhenBothPresent Nothing (Just q) = q
                andWhenBothPresent Nothing Nothing = True
            ReadEntry {..} <- maybeReadEntry
            return $
              andWhenBothPresent
                (isInfixOf <$> authorInfix <*> pure readEntryAuthor)
                (isInfixOf <$> titleInfix <*> pure readEntryTitle)
        defEntrySatisfies :: MonadIO m => Entity DefEntry -> DB m (Maybe Bool, Entity DefEntry)
        defEntrySatisfies =
          (\e@Entity {..} -> do
             mSatisfies <-
               maybe
                 (pure $ Just True)
                 (attributionSatisfies (Just "") (Just "Lighthouse"))
                 (defEntryAttributionTag entityVal)
             return (mSatisfies, e))
    defs <- selectList ([] :: [Filter DefEntry]) []
    satisfactoryDefs <- filter (maybe True id . fst)<$> traverse defEntrySatisfies defs
    liftIO $ traverse pPrint satisfactoryDefs

    --clear
    return ()

clear :: IO ()
clear =
  runSqlite "sqliteSpec.db" $ do
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
      , Read "Silas Marner" "George Eliot (Mary Ann Evans)")
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
