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
import qualified Data.Text as T
import           Data.Time                      ( UTCTime(..) )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Persist
import           Database.Persist.Sqlite
import           Debug.Trace
import           Helpers
import           Parse
import           Parse.Entry
import           Data.Semigroup                 ( (<>) )
import           Store.Sqlite
import           Test.Hspec
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



-- | Attribution filters: if the title and author infix lists are empty, these
-- are skipped over, otherwise they are the first in the chain of filters.
-- Eventually, we may refactor the filters into a single pass, should
-- performance become an issue.
--
-- Each record type/table/entry type will have its own custom filtration
-- pipeline: the first segment of each will be a switch to include or exclude
-- that entry type in the final list of (filtered) entries.
--
-- For instance, 'DefEntry`s will pass through the following filters in order:
--    1. entry type inclusion/exclusion;
--    3. date filtration (based on query's date range);
--    3. read attribution; and
--    4. definition variant :
--        * phrase | definition
--        * headwords | inline | comparision (a.t.m. implies the definition
--        filter).
--
--  More generally, the pipeline reduces to steps 1-3 as above and then some
--  entry variant specific filtration based on its unique fields. Note that
--  when we receive a query containing a predicate on a field not common to all
--  entry variants, only variants that have the field will be included in the
--  output (however in the first pass we should imitate the current
--  functionality as a kind of regression testing to see whether the new
--  pipeline has all the functionality of the old).
attributionSatisfies
  :: MonadIO m => [String] -> [String] -> Key ReadEntry -> DB m Bool
attributionSatisfies authorPreds titlePreds readKey = do
  maybeReadEntry <- get readKey
  return $
    -- if the entry has no read attribution and we are given author or
    -- title predicates, then the entry is omitted from the filtered
    -- output.
           case maybeReadEntry of
    Just ReadEntry {..} ->
      and (isInfixOf <$> authorPreds <*> pure readEntryAuthor)
        && and (isInfixOf <$> titlePreds <*> pure readEntryTitle)
    Nothing -> False

-- | Applies auth/title predicates to a quote's manual attribution. If no
-- manual attribution is present, @Nothing@ is returned.
manualAttributionSatisfies :: [String] -> [String] -> QuoteEntry -> Maybe Bool
manualAttributionSatisfies authorPreds titlePreds QuoteEntry { quoteEntryManualAttribution }
  = do
    attr <- quoteEntryManualAttribution
    return $ and (isInfixOf <$> authorPreds <*> pure attr) && and (isInfixOf <$> titlePreds <*> pure attr)



-- | Checks whether a single 'Entry'\'s title/author attribution satisfies the
-- given search strings.
taggedEntrySatisfies
  :: (MonadIO m, Tagged record)
  => [String]
  -> [String]
  -> Entity record
  -> DB m (Maybe (Entity record))
taggedEntrySatisfies authorPreds titlePreds entity@Entity {..} =
  case getAttributionTag entityVal of
    Just readKey -> do

      -- FIXME when an entry has an inferred attribution, by indentation, /and/
      -- a manual attribution, it satisfies the search--that is, it becomes a
      -- result--if the search predicates succeed for either attribution.

      satisfies <- attributionSatisfies authorPreds titlePreds readKey
      let manualSatisfies =
            maybe True id
              $   getQuoteEntry entityVal
              >>= manualAttributionSatisfies authorPreds titlePreds
      return $ if satisfies || manualSatisfies then Just entity else Nothing
    Nothing -> return Nothing

-- | Title and author search predicates test whether the given search strings
-- are all infixes of the title or author, respectively.
--
---- FIXME too many passes
applyReadPreds
  :: (MonadIO m, Tagged record)
  => [String] -- | Author predicates
  -> [String]
  -> [Entity record]
  -> DB m [Entity record]
applyReadPreds authorPreds titlePreds entities =
  fmap catMaybes
    $ sequence
    $ let x = taggedEntrySatisfies authorPreds titlePreds <$> entities
      in  trace ("applyReadPreds: \n" <> "# results: " <> show (length x)) x

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
  liftIO $ traverse pPrint satisfactoryDefs
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
