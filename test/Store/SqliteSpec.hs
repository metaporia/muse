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


import           CLI.Parser.Types               ( BoolExpr(..) )
import           Control.Lens                   ( (^.)
                                                , (^?)
                                                , _1
                                                , _Right
                                                , over
                                                , preview
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Either                    ( partitionEithers
                                                , rights
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text.IO                  as T
import           Data.Time                      ( UTCTime(..)
                                                , addDays
                                                )
import           Data.Time.Calendar             ( Day )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Esqueleto             ( (&&.)
                                                , (==.)
                                                , from
                                                , like
                                                , select
                                                , val
                                                , where_
                                                )
import qualified Database.Esqueleto            as E
import           Database.Persist        hiding ( (==.) )
import           Database.Persist.Sqlite hiding ( (==.) )
import           Helpers
import           Lib                            ( testSearchConfig )
import           Parse                          ( parseLogEntries )
import           Parse.Types
import qualified Parse.Types                   as P
import           Render                         ( showAll )
import           Store                          ( Result(..) )
import           Store.Sqlite            hiding ( InlineDef )
import           Store.Sqlite.Types             ( Tags(..) )
import           Test.Hspec              hiding ( before )
import           Test.Hspec.Megaparsec          ( shouldParse )
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import           Time

asIO :: IO a -> IO a
asIO a = a

test :: IO ()
test = hspec spec

setup writeAction = do
  runMigrationSilent migrateAll
  today  <- liftIO $ utctDay <$> getCurrentTime
  xs     <- writeAction today
  before <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since  <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime
  return (since, before, xs)


spec :: Spec
spec = do
  let filterQuotesSetup = setup (`writeDay` demoLogEntries)
      defVarSetup       = setup
        (\today -> do
          let entries = fromJust $ (parseLogEntries defVar) ^. _1 ^? _Right
          writeDay today entries
        )

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
      (since, before, _) <- filterQuotesSetup
      matchingQuotes     <- filterQuotes since
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
    it "all def variants" $ asIO $ runSqlInMem $ do
      (since, before, _) <- filterQuotesSetup
      defs               <- filterDefs'
        since
        before
        []
        []
        []
        (DefSearch allDefVariants Nothing Nothing)
      liftIO $ resultsToEntry (rights defs) `shouldBe` demoDefsAll
    it "def versus variants" $ asIO $ runSqlInMem $ do
      (since, before, _) <- filterQuotesSetup
      defs               <- filterDefs' since
                                        before
                                        []
                                        []
                                        []
                                        (DefSearch [DefVersus'] Nothing Nothing)
      liftIO $ resultsToEntry (rights defs) `shouldBe` demoDefVersus
    it "vanilla definition variants" $ asIO $ runSqlInMem $ do
      (since, before, _) <- filterQuotesSetup
      defs <- filterDefs' since before [] [] [] (DefSearch [Defn'] Nothing Nothing)
      liftIO $ resultsToEntry (rights defs) `shouldBe` demoDefn
    it "inline def definition variants" $ asIO $ runSqlInMem $ do
      (since, before, _) <- defVarSetup
      defs               <- filterDefs' since
                                        before
                                        []
                                        []
                                        []
                                        (DefSearch [InlineDef'] Nothing Nothing)
      liftIO $ resultsToEntry (rights defs) `shouldBe` varInline
    it "phrase definition variants" $ asIO $ runSqlInMem $ do
      (since, before, _) <- defVarSetup
      defs               <- filterDefs' since
                                        before
                                        []
                                        []
                                        ["phrase"]
                                        (DefSearch allDefVariants Nothing Nothing)
      liftIO $ resultsToEntry (rights defs) `shouldBe` varPhrase
  testSearchDispatch






getDateRange :: MonadIO m => m (Day, Day)
getDateRange = do
  before <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since  <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime
  return (since, before)

setup'' file = setup $ \today -> do
  exampleMuseLog <- liftIO $ T.readFile file
  let entries = fromJust $ parseLogEntries exampleMuseLog ^. _1 ^? _Right
  partitionEithers <$> writeDay today entries

dispatchSearch'
  :: MonadIO m => SearchConfig -> DB m ([String], [(TimeStamp, Entry)])
dispatchSearch' = -- fmap (fmap dropFirst . asTimestamped) .
  fmap (fmap (fmap dropFirst . asTimestamped) . partitionEithers)
    . dispatchSearch "/home/aporia/.muse/log"

run = hspec testSearchDispatch

testSearchDispatch :: SpecWith ()
testSearchDispatch = describe "dispatchSearch" $ do
  let setup' = setup'' "examples/18.11.24-corrected"
  it "only test date range"
    $ asIO
    $ runSqlInMem
    $ do
        let debug = False
        (since, before, (parseErrs, _)) <- setup'
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)

        -- these are /only/ for explicitly set entry variant filters.
        -- They are expected to have been set by the parser.
        let searchAllBare before' = SearchConfig
              since
              before'
              False -- dump support unimplemented as yet
              False -- quotes
              False -- dialogues
              False -- comments
              False -- definitions
              ([], []) -- (authPreds, titlePreds)
              (DefSearch [] Nothing Nothing) -- defQueryVariants, headwordPreds, meaningPreds
              [] -- quote body search strings
              [] -- commentary ^
              [] -- dialogue   ^
              [] -- dump       ^ (ignore)
              []
        (searchErrs, results) <- dispatchSearch' (searchAllBare before)
        when
          debug
          (if not $ null searchErrs
            then liftIO $ pPrint' parseErrs
            else liftIO $ pPrint' results
          )
        -- should return all
        liftIO $ results `shouldBe` dispatchAllBare
        (searchErrs, results) <- dispatchSearch' (searchAllBare since)
        -- should return nothing
        liftIO $ results `shouldBe` []
  it "definition filters"
    $ asIO
    $ runSqlInMem
    $ do
        let debug  = False
            pretty = False
        (since, before, (parseErrs, _)) <- setup'
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search variants = SearchConfig
              since
              before
              False -- dump support unimplemented as yet
              False -- quotes
              False -- dialogues
              False -- comments
              True -- definitions
              ([], []) -- (authPreds, titlePreds)
              (DefSearch variants Nothing Nothing) -- defQueryVariants, headwordPreds, meaningPreds
              [] -- quote body search strings
              [] -- commentary ^
              [] -- dialogue   ^
              [] -- dump       ^ (ignore)
              [] -- tags
        -- all variants
        (searchErrs, results) <- dispatchSearch' (search allDefVariants)

        liftIO $ results `shouldBe` dispatchOnlyDefs

        -- no variants
        (searchErrs, results) <- dispatchSearch' (search [])

        liftIO $ results `shouldBe` dispatchOnlyDefs

        ---- only inlines
        --(searchErrs, results) <- dispatchSearch' (search [InlineDef'])

        --liftIO $ results `shouldBe` dispatchOnlyInline

        ---- only defversus
        --(searchErrs, results) <- dispatchSearch' (search [DefVersus'])

        --liftIO $ results `shouldBe` dispatchOnlyDefVersus

        when
          debug
          (if not $ null searchErrs
            then liftIO $ pPrint' parseErrs
            else if pretty
              then liftIO $ showAll $ fmap snd results
              else liftIO $ pPrint' results
          )
  it "quote filters (with only one author)"
    $ asIO
    $ runSqlInMem
    $ do
        let debug  = False
            pretty = False
        (since, before, (parseErrs, _)) <- setup'
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search auth title filters = SearchConfig
              since
              before
              False -- dump support unimplemented as yet
              True -- quotes
              False -- dialogues
              False -- comments
              False -- definitions
              (auth, title) -- (authPreds, titlePreds)
              (DefSearch [] Nothing Nothing) -- defQueryVariants, headwordPreds, meaningPreds
              filters -- quote body search strings
              [] -- commentary ^
              [] -- dialogue   ^
              [] -- dump       ^ (ignore)
              []
        -- all quotes, no body preds
        (searchErrs, results) <- dispatchSearch' (search [] [] [])

        liftIO $ results `shouldBe` dispatchOnlyQuotes

        ---- one body pred
        (searchErrs, results) <- dispatchSearch' (search [] [] ["%Clevinger%"])

        liftIO $ results `shouldBe` dispatchOneQuotePred

        -- two quote preds (AND)
        (searchErrs, results) <- dispatchSearch'
          (search [] [] ["%Clevinger%", "%basic flaw%"])

        liftIO $ results `shouldBe` dispatchOneQuotePred

        ----  auth pred
        (searchErrs, results) <- dispatchSearch' (search ["%Heller%"] [] [])

        liftIO $ results `shouldBe` dispatchOnlyQuotes

        --- author and one quote preds
        (searchErrs, results) <- dispatchSearch'
          (search ["%Heller%"] [] ["%was%"])

        liftIO $ results `shouldBe` dispatchAuthBody

        --- author, title, and one quote preds
        (searchErrs, results) <- dispatchSearch'
          (search [] ["%Catch-22%"] ["%was%"])
        liftIO $ results `shouldBe` dispatchAuthBody

        when
          debug
          (if not $ null searchErrs
            then liftIO $ pPrint' parseErrs
            else if pretty
              then liftIO $ showAll $ fmap snd results
              else liftIO $ pPrint' results
          )
  it "quote filters (with many authors)"
    $ asIO
    $ runSqlInMem
    $ do
        let debug  = False
            pretty = False
        (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search auth title filters = SearchConfig
              since
              before
              False -- dump support unimplemented as yet
              True -- quotes
              False -- dialogues
              False -- comments
              False -- definitions
              (auth, title) -- (authPreds, titlePreds)
              (DefSearch [] Nothing Nothing) -- defQueryVariants, headwordPreds, meaningPreds
              filters -- quote body search strings
              [] -- commentary ^
              [] -- dialogue   ^
              [] -- dump       ^ (ignore)
              []
        -- all quotes, no body preds
        (searchErrs, results) <- dispatchSearch' (search [] [] [])

        liftIO $ results `shouldBe` globLogAllQuotes

        -- austen qs
        (searchErrs, results) <- dispatchSearch' (search ["%Austen%"] [] [])

        liftIO $ results `shouldBe` globAustenQs

        -- austen emma quote
        (searchErrs, results) <- dispatchSearch'
          (search ["%Austen%"] [] ["%misapply%"])

        liftIO $ results `shouldBe` globAustenEmma

        -- austen, emma
        (searchErrs, results) <- dispatchSearch'
          (search ["%Austen%"] ["%Emma%"] [])

        liftIO $ results `shouldBe` globAustenEmma

        ------  auth pred
        --(searchErrs, results) <- dispatchSearch' (search ["%Heller%"] [] [])
        --
        --liftIO $ results `shouldBe` dispatchOnlyQuotes
        --
        ----- author and one quote preds
        --(searchErrs, results) <- dispatchSearch' (search ["%Heller%"] [] ["%was%"])
        --
        --liftIO $ results `shouldBe` dispatchAuthBody
        --
        ----- author, title, and one quote preds
        --(searchErrs, results) <- dispatchSearch' (search [] ["%Catch-22%"] ["%was%"])
        --liftIO $ results `shouldBe` dispatchAuthBody

        when
          debug
          (if not $ null searchErrs
            then liftIO $ pPrint' parseErrs
            else if pretty
              then liftIO $ showAll $ fmap snd results
              else liftIO $ pPrint' results
          )

  it "dialogue"
    $ asIO
    $ runSqlInMem
    $ do
        (since, before, (parseErrs, _)) <- setup'' "examples/18.11.24-corrected"
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search filters auth title = SearchConfig
              since
              before
              False
              False
              True
              False
              False
              (auth, title)
              (DefSearch [] Nothing Nothing)
              []
              []
              filters
              []
              []

        -- all dialogues
        (searchErrs, results) <- dispatchSearch' (search [] [] [])
        liftIO $ results `shouldBe` dialogueExp

        -- dialogue body pred
        (searchErrs, results) <- dispatchSearch' (search ["%sql%"] [] [])
        liftIO $ results `shouldBe` dialogueExp


        -- dialogues under catch 22
        (searchErrs, results) <- dispatchSearch' (search [] ["%Heller%"] [])
        liftIO $ results `shouldBe` dialogueExp

        -- dialogues under catch 22 with body pred
        (searchErrs, results) <- dispatchSearch'
          (search ["%sql%"] ["%Heller%"] [])
        liftIO $ results `shouldBe` dialogueExp

        -- dialogues under austen
        (searchErrs, results) <- dispatchSearch' (search [] ["%Austen%"] [])
        liftIO $ results `shouldBe` []

  it "commentary"
    $ asIO
    $ runSqlInMem
    $ do
        (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search filters auth title = SearchConfig
              since
              before
              False
              False
              False
              True
              False
              (auth, title)
              (DefSearch [] Nothing Nothing)
              []
              filters
              []
              []
              []

        -- all comments
        (searchErrs, results) <- dispatchSearch' (search [] [] [])

        liftIO $ results `shouldBe` globCommentLexical

        -- one comment body predicate
        (searchErrs, results) <- dispatchSearch' (search ["%lexical%"] [] [])

        liftIO $ results `shouldBe` take 1 globCommentLexical

        -- commentsby author
        (searchErrs, results) <- dispatchSearch' (search [] ["%Robbins%"] [])
        liftIO $ results `shouldBe` take 1 globCommentLexical

        (searchErrs, results) <- dispatchSearch'
          (search [] [] ["%Northanger Abbey%"])
        liftIO $ results `shouldBe` [globCommentLexical !! 1]

        (searchErrs, results) <- dispatchSearch'
          (search ["%lexical%"] ["%Austen%"] [])
        liftIO $ results `shouldBe` []

        (searchErrs, results) <- dispatchSearch'
          (search ["%jettisoned%"] ["%Austen%"] [])
        liftIO $ results `shouldBe` [globCommentLexical !! 1]

  it "definition and phrase headword search"
    $ asIO
    $ runSqlInMem
    $ do
        (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search headwords mn defs defnVariants tags = SearchConfig
              since
              before
              False
              False
              False
              False
              defs -- include defs?
              ([], [])
              (DefSearch defnVariants (Just headwords) mn)
              []
              []
              []
              []
              tags
        (searchErrs, results) <- dispatchSearch'
          (search (LitE (InfixSearch "twaddle")) Nothing True [Defn'] [])
        liftIO
          $          results
          `shouldBe` [ ( TimeStamp {hr = 19, min = 7, sec = 0}
                       , Def (Defn Nothing ["twaddle", "twattle"])
                       )
                     ]
        (searchErrs, results) <- dispatchSearch'
          (search (LitE (InfixSearch "fail of"))
                  Nothing
                  False
                  allDefVariants
                  ["phrase"]
          )
        liftIO
          $          results
          `shouldBe` [ ( TimeStamp {hr = 8, min = 51, sec = 44}
                       , Def (Defn Nothing ["\"omit to\"", "\"fail of\""])
                       )
                     ]
        (searchErrs, results) <- dispatchSearch'
          (search
            (OrE (LitE (PrefixSearch "brim")) (LitE (PrefixSearch "sanguin")))
            Nothing
            False
            [Defn']
            []
          )
        liftIO
          $          results
          `shouldBe` [ ( TimeStamp {hr = 8, min = 50, sec = 4}
                       , Def (Defn Nothing ["brimful"])
                       )
                     , ( TimeStamp {hr = 8, min = 51, sec = 33}
                       , Def (Defn Nothing ["sanguinary"])
                       )
                     ]

        (searchErrs, results) <- dispatchSearch'
          (search
            (LitE (InfixSearch ""))
            (Just
              (OrE (LitE (SuffixSearch "worry"))
                   (LitE (InfixSearch "inheritance"))
              )
            )
            False
            allDefVariants
            []
          )
        liftIO
          $          results
          `shouldBe` [ ( TimeStamp {hr = 8, min = 53, sec = 38}
                       , Def
                         (InlineDef
                           "pother"
                           "(n.) bustle, tumult, bother; (v.) to perplex, worry"
                         )
                       )
                     , ( TimeStamp {hr = 12, min = 45, sec = 24}
                       , Def
                         (InlineDef "patrimony"
                                    "an inheritance from one's father"
                         )
                       )
                     ]





  it "phrases & quotes queries"
    $ asIO
    $ runSqlInMem
    $ do
        let debug  = False
            pretty = False
        (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
        liftIO $ unless (null parseErrs) (pPrint' parseErrs)
        let search auth title variants qs tags = SearchConfig
              since
              before
              False -- dump support unimplemented as yet
              True  -- quotes
              False -- dialogues
              False -- comments
              True -- definitions
              (auth, title) -- (authPreds, titlePreds)
              (DefSearch variants Nothing Nothing) -- defQueryVariants, headwordPreds, meaningPreds
              qs -- quote body search strings
              [] -- commentary ^
              [] -- dialogue   ^
              [] -- dump       ^ (ignore)
              []
        -- phrases and quotes by eliot
        config <- liftIO $ fromJust <$> testSearchConfig "-q --phrases -aEliot"
        (searchErrs, results) <- dispatchSearch' config


        liftIO $ results `shouldBe` globMultiPhrQt

        -- defversus and quotes matching "holiness"
        (searchErrs, results) <- dispatchSearch'
          (search [] [] [DefVersus'] ["%holiness%"] [])

        liftIO $ results `shouldBe` globMultiDefVsQt

        when
          debug
          (if not $ null searchErrs
            then liftIO $ pPrint' parseErrs
            else if pretty
              then liftIO $ showAll $ fmap snd results
              else liftIO $ pPrint' results
          )
  it "write tagged defns"
    $ asIO
    $ runSqlInMem
    $ do
        runMigrationSilent migrateAll
        today                 <- liftIO $ utctDay <$> getCurrentTime
        timestamps            <- writeDay today tagDemoEntries
        -- search for "phrase"
        searchConfig <- liftIO $ fromJust <$> testSearchConfig "--tags phrase"
        (searchErrs, results) <- dispatchSearch' searchConfig
        let expected =
              [ ( TimeStamp {hr = 10, min = 17, sec = 40}
                , Def (Defn Nothing ["inimical", "traduce", "virulent"])
                )
              ]
        liftIO $ results `shouldBe` expected
        -- search for "some_tag"
        searchConfig <- liftIO $ fromJust <$> testSearchConfig "--tags some_tag"
        (searchErrs, results) <- dispatchSearch' searchConfig
        liftIO
          $          results
          `shouldBe` [ ( TimeStamp {hr = 10, min = 18, sec = 12}
                       , Def (Defn Nothing ["sublime", "lintel"])
                       )
                     ]






pSearch search = runSqlInMem $ do
  (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
  liftIO $ unless (null parseErrs) (pPrint' parseErrs)
  -- (searchErrs, results) <-  (searchAllBare before)
  (searchErrs, results) <- dispatchSearch' search
  liftIO $ showAll $ fmap snd results




  --it "multi-variant queries"
  --  $ asIO
  --  $ runSqlInMem
  --  $ do
  --      let debug  = False
  --          pretty = False
  --      (since, before, (parseErrs, _)) <- setup'' "examples/globLog"
  --      liftIO $ unless (null parseErrs) (pPrint' parseErrs)
  --      let search auth title variants qs = SearchConfig
  --            since
  --            before
  --            False -- dump support unimplemented as yet
  --            True  -- quotes
  --            False -- dialogues
  --            False -- comments
  --            True -- definitions
  --            (auth, title) -- (authPreds, titlePreds)
  --            (DefSearch variants [] []) -- defQueryVariants, headwordPreds, meaningPreds
  --            qs -- quote body search strings
  --            [] -- commentary ^
  --            [] -- dialogue   ^
  --            [] -- dump       ^ (ignore)
  --      -- phrases and quotes by eliot
  --      (searchErrs, results) <- dispatchSearch'
  --        (search ["%Eliot%"] [] [Phrase'] [])

  --      liftIO $ results `shouldBe` globMultiPhrQt

  --      -- defversus and quotes matching "holiness"
  --      (searchErrs, results) <- dispatchSearch'
  --        (search [] [] [DefVersus'] ["%holiness%"])

  --      liftIO $ results `shouldBe` globMultiDefVsQt

  --      when
  --        debug
  --        (if not $ null searchErrs
  --          then liftIO $ pPrint' parseErrs
  --          else if pretty
  --            then liftIO $ showAll $ fmap snd results
  --            else liftIO $ pPrint' results
  --        )


runSqlInMem = runSqliteInfo $ mkSqliteConnectionInfo ":memory:"


dropFirst :: (a, b, c) -> (b, c)
dropFirst (a, b, c) = (b, c)

asTimestamped :: [Result] -> [(Day, TimeStamp, Entry)]
asTimestamped = mapMaybe
  (\r -> case r of
    TsR utc e -> let (day, ts) = fromUTC utc in Just (day, ts, e)
    _         -> Nothing
  )

resultToEntry :: Result -> Maybe Entry
resultToEntry (TsR _ entry) = Just entry
resultToEntry _             = Nothing

resultsToEntry :: [Result] -> [Entry]
resultsToEntry = mapMaybe resultToEntry

run' :: IO ()
run' = runSqlInMem $ do
  runMigrationSilent migrateAll
  today <- liftIO $ utctDay <$> getCurrentTime
  writeDay today demoLogEntries
  before         <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since          <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime

  matchingQuotes <- rights <$> filterQuotes' since before [] [] []
  defs <- rights <$> filterDefs' since
                                 before
                                 []
                                 []
                                 []
                                 (DefSearch allDefVariants Nothing Nothing)
--  liftIO $ pPrint $ mapMaybe resultToEntry defs
  liftIO $ showAll $ mapMaybe resultToEntry defs
  return ()

test' :: String -> IO ()
test' x =
  runResourceT
    . runStdoutLoggingT
    . withSqliteConn "/home/aporia/.muse/state/sqlite.db"
    . runSqlConn
    $ do
        before   <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
        since    <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime
        matching <- filterQuotes since before [] [] ["%" <> x <> "%"]
        liftIO $ traverse_ (pPrint . quoteEntryBody . entityVal) matching
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
  before    <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since     <- liftIO $ addDays (-6 * 30) . utctDay <$> getCurrentTime

  olderTime <- incrUTCBy (-5) <$> liftIO getCurrentTime
  newerTime <- liftIO getCurrentTime
  -- FIXME printing the same time atm
  liftIO $ pPrint (olderTime, newerTime)
  allParseTimes <- selectList ([] :: [Filter LastParse]) []
  liftIO $ traverse_ (pPrint . entityKey) allParseTimes
  setLastParseTime olderTime True
  t0 <- getLastParseTime
  liftIO $ putStr "olderTime: " >> pPrint t0
  setLastParseTime newerTime True
  t1 <- getLastParseTime
  liftIO $ putStr "newerTime: " >> pPrint t1

  matchingQuotes <- filterQuotes since
                                 before
                                 ["%Woolf%"]
                                 ["%Lighthouse%"]
                                 ["%simplicity%"]
  liftIO $ pPrint $ quoteEntryBody . entityVal <$> matchingQuotes -- -- satisfactoryDefs
  --let newRead = ReadEntry {readEntryTitle = "Title", readEntryAuthor = "Author"} liftIO $ putStrLn "before repsert"
  --selectList ([] :: [Filter ReadEntry]) [] >>= liftIO . traverse_
  --  (pPrint . entityVal)
  --repsert readKey newRead
  --liftIO $ putStrLn "after repsert"
  --selectList ([] :: [Filter ReadEntry]) [] >>= liftIO . traverse_
  --  (pPrint . entityVal)
  defs <- filterDefs' since before [] [] [] (DefSearch [Defn'] Nothing Nothing)
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
  deleteWhere ([] :: [Filter LastParse])
  deleteWhere ([] :: [Filter DumpEntry])


tagDemoEntries =
  [ TabTsEntry
    ( 1
    , TimeStamp {hr = 10, min = 17, sec = 40}
    , Def (Defn (Just 38) ["inimical", "traduce", "virulent"])
    , Tags ["other", "phrase", "wodehouse"]
    )
  , TabTsEntry
    ( 1
    , TimeStamp {hr = 10, min = 18, sec = 12}
    , Def (Defn (Just 38) ["sublime", "lintel"])
    , Tags ["some_tag"]
    )
  ]

demoLogEntries :: [LogEntry]
demoLogEntries
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf"
      , Tags []
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
      , Tags []
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
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Def (Defn (Just 38) ["inimical", "traduce", "virulent"])
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"])
      , Tags []
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 23, sec = 0}
      , Read "Silas Marner" "George Eliot (Mary Ann Evans)"
      , Tags []
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
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation "Her simplicity fathomed what clever people falsified."
                  "In \"To the Lighthouse\", by Virginia Woolf"
                  Nothing
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"])
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"])
      , Tags []
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf"
        (Just 38)
      , Tags []
      )
    ]


--curr :: IO ()
--curr = runSqlite "test1.db" $ do
--  runMigration migrateAll
--  today <- liftIO $ utctDay <$> getCurrentTime
--  let -- logEntries :: _
--      logEntries' = statefulValidatedMany Nothing
--                                          logEntrySquawk
--                                          logEntryPassNewestTimeStamp
--  liftIO $ pPrint $ parse logEntries' reallyBroken
--  --writeDay today $ fromJust $ toMaybe $ parse logEntries reallyBroken
--  return ()

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
  [ Def (Defn Nothing ["\"fever of suspense\""])
  , Def (Defn Nothing ["\"smarting under an obligation\""])
  ]

dispatchAllBare =
  [ ( TimeStamp {hr = 10, min = 7, sec = 15}
    , Def
      (Defn
        Nothing
        ["lackadaisical", "farinaceous", "idyll", "moue", "furgle", "svelte"]
      )
    )
  , (TimeStamp {hr = 10, min = 7, sec = 44}, Def (Defn Nothing ["filip"]))
  , (TimeStamp {hr = 10, min = 8, sec = 55}, Def (Defn Nothing ["afflatus"]))
  , ( TimeStamp {hr = 10, min = 9, sec = 26}
    , Def (InlineDef "afflate" "(obs.) to inspire; to blow upon")
    )
  , ( TimeStamp {hr = 10, min = 9, sec = 37}
    , Def
      (Defn
        Nothing
        [ "temerity"
        , "intransigent"
        , "concupiscent"
        , "denudate"
        , "crump"
        , "piaster"
        ]
      )
    )
  , ( TimeStamp {hr = 10, min = 9, sec = 57}
    , Def
      (Defn
        Nothing
        ["rubicund", "cataleptic", "otiose", "argosy", "ersatz", "stentorian"]
      )
    )
  , (TimeStamp {hr = 10, min = 10, sec = 21}, Def (Defn Nothing ["fustian"]))
  , ( TimeStamp {hr = 10, min = 10, sec = 30}
    , Def
      (DefVersus
        "umber"
        "brown or reddish pigment"
        "ochre"
        "red (hematite) or yellow (limonite) pigment; the\ncolor is near orange"
      )
    )
  , ( TimeStamp {hr = 10, min = 14, sec = 28}
    , Def
      (DefVersus "flotsam"
                 "goods which float when lost at sea"
                 "jetsam"
                 "goods which sink"
      )
    )
  , (TimeStamp {hr = 10, min = 18, sec = 41}, Def (Defn Nothing ["lissome"]))
  , ( TimeStamp {hr = 13, min = 25, sec = 38}
    , Def (Defn Nothing ["incarnadine", "maudlin "])
    )
  , ( TimeStamp {hr = 13, min = 26, sec = 41}
    , Def (InlineDef "savoir faire" "(lit.) know-how; sauvity; social grace")
    )
  , ( TimeStamp {hr = 13, min = 27, sec = 30}
    , Def (Defn Nothing ["tameless", "foredoom", "hirsute", "cadaverous"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 29}
    , Def (Defn Nothing ["wroth", "sedulous"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 48}
    , Def (Defn Nothing ["parturition"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 58}
    , Def (Defn Nothing ["mucid", "mucous (adj.!)"])
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 6}
    , Def
      (Defn
        Nothing
        ["versicolor", "andiron", "scrod", "tripe", "vociferous", "myopic"]
      )
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 32}
    , Def (Defn Nothing ["grist", "homily", "apothegm", "Wac", "blas\233"])
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 53}
    , Def (InlineDef "callipygous" "having beautiful buttocks")
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 57}
    , Def (Defn Nothing ["pullulate", "pullet"])
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 3}
    , Def
      (DefVersus
        "dissent (n.)"
        "the act of dissenting, disagreement, etc."
        "dissension (n.)"
        "disagreement of a violent character;\nstrife; quarrel; discord."
      )
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 17}
    , Def (Defn Nothing ["spook", "rapacious", "equine", "clannish"])
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 34}
    , Def (Defn Nothing ["calcareous", "musette", "cameo"])
    )
  , ( TimeStamp {hr = 19, min = 0, sec = 35}
    , Def (Defn Nothing ["spurt", "sprit"])
    )
  , ( TimeStamp {hr = 19, min = 8, sec = 52}
    , Def (InlineDef "sorrel" "of a yellowish or reddish brown color")
    )
  , ( TimeStamp {hr = 19, min = 11, sec = 25}
    , Def
      (InlineDef
        "phlegmatic"
        "watery; generating or causing phlegm; or, of a\nperson, not easily excited to action"
      )
    )
  , ( TimeStamp {hr = 19, min = 12, sec = 8}
    , Def
      (DefVersus
        "ployment"
        "the act of forming up a body of troops into, e.g., a column "
        "deployment"
        "the act of spreading a body of troops about a\nfront; (modern) resource arrangement or\ndistribution in preparation for battel or\nwork."
      )
    )
  , ( TimeStamp {hr = 19, min = 25, sec = 25}
    , Def
      (InlineDef "clasm"
                 "(colloq.) orgasm or, literally, breakage (as in \"-clast\")"
      )
    )
  , ( TimeStamp {hr = 10, min = 16, sec = 17}
    , Quotation
      "...he did not hate his mother and father, even though they had both\nbeen very good to him."
      "In \"Catch-22\" by Joseph Heller"
      Nothing
    )
  , ( TimeStamp {hr = 10, min = 17, sec = 9}
    , Quotation
      "Clevinger was dead. That was the basic flaw in his philosophy."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 19, min = 15, sec = 48}
    , Quotation
      "He was pinched persipiringly in the epistemological dilemma of the\nskeptic, unable to accept solutions to problems he was unwilling to\ndismiss as unsolvable."
      "In \"Catch-22\" by Joseph Heller"
      Nothing
    )
  , ( TimeStamp {hr = 19, min = 15, sec = 49}
    , Dialogue "ATTICUS: SQL!\n\n(deafening silence)"
    )
  ]

dialogueExp =
  [ ( TimeStamp {hr = 19, min = 15, sec = 49}
    , Dialogue "ATTICUS: SQL!\n\n(deafening silence)"
    )
  ]

dispatchOnlyDefs =
  [ ( TimeStamp {hr = 10, min = 7, sec = 15}
    , Def
      (Defn
        Nothing
        ["lackadaisical", "farinaceous", "idyll", "moue", "furgle", "svelte"]
      )
    )
  , (TimeStamp {hr = 10, min = 7, sec = 44}, Def (Defn Nothing ["filip"]))
  , (TimeStamp {hr = 10, min = 8, sec = 55}, Def (Defn Nothing ["afflatus"]))
  , ( TimeStamp {hr = 10, min = 9, sec = 26}
    , Def (InlineDef "afflate" "(obs.) to inspire; to blow upon")
    )
  , ( TimeStamp {hr = 10, min = 9, sec = 37}
    , Def
      (Defn
        Nothing
        [ "temerity"
        , "intransigent"
        , "concupiscent"
        , "denudate"
        , "crump"
        , "piaster"
        ]
      )
    )
  , ( TimeStamp {hr = 10, min = 9, sec = 57}
    , Def
      (Defn
        Nothing
        ["rubicund", "cataleptic", "otiose", "argosy", "ersatz", "stentorian"]
      )
    )
  , (TimeStamp {hr = 10, min = 10, sec = 21}, Def (Defn Nothing ["fustian"]))
  , ( TimeStamp {hr = 10, min = 10, sec = 30}
    , Def
      (DefVersus
        "umber"
        "brown or reddish pigment"
        "ochre"
        "red (hematite) or yellow (limonite) pigment; the\ncolor is near orange"
      )
    )
  , ( TimeStamp {hr = 10, min = 14, sec = 28}
    , Def
      (DefVersus "flotsam"
                 "goods which float when lost at sea"
                 "jetsam"
                 "goods which sink"
      )
    )
  , (TimeStamp {hr = 10, min = 18, sec = 41}, Def (Defn Nothing ["lissome"]))
  , ( TimeStamp {hr = 13, min = 25, sec = 38}
    , Def (Defn Nothing ["incarnadine", "maudlin "])
    )
  , ( TimeStamp {hr = 13, min = 26, sec = 41}
    , Def (InlineDef "savoir faire" "(lit.) know-how; sauvity; social grace")
    )
  , ( TimeStamp {hr = 13, min = 27, sec = 30}
    , Def (Defn Nothing ["tameless", "foredoom", "hirsute", "cadaverous"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 29}
    , Def (Defn Nothing ["wroth", "sedulous"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 48}
    , Def (Defn Nothing ["parturition"])
    )
  , ( TimeStamp {hr = 18, min = 57, sec = 58}
    , Def (Defn Nothing ["mucid", "mucous (adj.!)"])
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 6}
    , Def
      (Defn
        Nothing
        ["versicolor", "andiron", "scrod", "tripe", "vociferous", "myopic"]
      )
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 32}
    , Def (Defn Nothing ["grist", "homily", "apothegm", "Wac", "blas\233"])
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 53}
    , Def (InlineDef "callipygous" "having beautiful buttocks")
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 57}
    , Def (Defn Nothing ["pullulate", "pullet"])
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 3}
    , Def
      (DefVersus
        "dissent (n.)"
        "the act of dissenting, disagreement, etc."
        "dissension (n.)"
        "disagreement of a violent character;\nstrife; quarrel; discord."
      )
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 17}
    , Def (Defn Nothing ["spook", "rapacious", "equine", "clannish"])
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 34}
    , Def (Defn Nothing ["calcareous", "musette", "cameo"])
    )
  , ( TimeStamp {hr = 19, min = 0, sec = 35}
    , Def (Defn Nothing ["spurt", "sprit"])
    )
  , ( TimeStamp {hr = 19, min = 8, sec = 52}
    , Def (InlineDef "sorrel" "of a yellowish or reddish brown color")
    )
  , ( TimeStamp {hr = 19, min = 11, sec = 25}
    , Def
      (InlineDef
        "phlegmatic"
        "watery; generating or causing phlegm; or, of a\nperson, not easily excited to action"
      )
    )
  , ( TimeStamp {hr = 19, min = 12, sec = 8}
    , Def
      (DefVersus
        "ployment"
        "the act of forming up a body of troops into, e.g., a column "
        "deployment"
        "the act of spreading a body of troops about a\nfront; (modern) resource arrangement or\ndistribution in preparation for battel or\nwork."
      )
    )
  , ( TimeStamp {hr = 19, min = 25, sec = 25}
    , Def
      (InlineDef "clasm"
                 "(colloq.) orgasm or, literally, breakage (as in \"-clast\")"
      )
    )
  ]

dispatchOnlyInline =
  [ ( TimeStamp {hr = 10, min = 9, sec = 26}
    , Def (InlineDef "afflate" "(obs.) to inspire; to blow upon")
    )
  , ( TimeStamp {hr = 13, min = 26, sec = 41}
    , Def (InlineDef "savoir faire " "(lit.) know-how; sauvity; social grace")
    )
  , ( TimeStamp {hr = 18, min = 58, sec = 53}
    , Def (InlineDef "callipygous" "having beautiful buttocks")
    )
  , ( TimeStamp {hr = 19, min = 8, sec = 52}
    , Def (InlineDef "sorrel" "of a yellowish or reddish brown color")
    )
  , ( TimeStamp {hr = 19, min = 11, sec = 25}
    , Def
      (InlineDef
        "phlegmatic"
        "watery; generating or causing phlegm; or, of a person, not easily excited to action"
      )
    )
  , ( TimeStamp {hr = 19, min = 25, sec = 25}
    , Def
      (InlineDef "clasm"
                 "(colloq.) orgasm or, literally, breakage (as in \"-clast\")"
      )
    )
  ]


dispatchOnlyDefVersus =
  [ ( TimeStamp {hr = 10, min = 10, sec = 30}
    , Def
      (DefVersus
        "umber"
        "brown or reddish pigment"
        "ochre"
        "red (hematite) or yellow (limonite) pigment; the color is near orange"
      )
    )
  , ( TimeStamp {hr = 10, min = 14, sec = 28}
    , Def
      (DefVersus "flotsam"
                 "goods which float when lost at sea"
                 "jetsam"
                 "goods which sink"
      )
    )
  , ( TimeStamp {hr = 18, min = 59, sec = 3}
    , Def
      (DefVersus
        "dissent (n.)"
        "the act of dissenting, disagreement, etc."
        "dissension (n.)"
        "disagreement of a violent character; strife; quarrel; discord."
      )
    )
  , ( TimeStamp {hr = 19, min = 12, sec = 8}
    , Def
      (DefVersus
        "ployment"
        "the act of forming up a body of troops into, e.g., a column"
        "deployment"
        "the act of spreading a body of troops about a front; (modern) resource arrangement or distribution in preparation for battel or work."
      )
    )
  ]

dispatchOnlyQuotes =
  [ ( TimeStamp {hr = 10, min = 16, sec = 17}
    , Quotation
      "...he did not hate his mother and father, even though they had both\nbeen very good to him."
      "In \"Catch-22\" by Joseph Heller"
      Nothing
    )
  , ( TimeStamp {hr = 10, min = 17, sec = 9}
    , Quotation
      "Clevinger was dead. That was the basic flaw in his philosophy."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 19, min = 15, sec = 48}
    , Quotation
      "He was pinched persipiringly in the epistemological dilemma of the\nskeptic, unable to accept solutions to problems he was unwilling to\ndismiss as unsolvable."
      "In \"Catch-22\" by Joseph Heller"
      Nothing
    )
  ]

dispatchOneQuotePred =
  [ ( TimeStamp {hr = 10, min = 17, sec = 9}
    , Quotation
      "Clevinger was dead. That was the basic flaw in his philosophy."
      ""
      Nothing
    )
  ]

dispatchAuthBody =
  [ ( TimeStamp {hr = 10, min = 17, sec = 9}
    , Quotation
      "Clevinger was dead. That was the basic flaw in his philosophy."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 19, min = 15, sec = 48}
    , Quotation
      "He was pinched persipiringly in the epistemological dilemma of the\nskeptic, unable to accept solutions to problems he was unwilling to\ndismiss as unsolvable."
      "In \"Catch-22\" by Joseph Heller"
      Nothing
    )
  ]

globLogAllQuotes =
  [ ( TimeStamp {hr = 8, min = 47, sec = 48}
    , Quotation
      "What novelty is worth that sweet monotony where everything is known,\nand _loved_ because it is known?"
      ""
      Nothing
    )
  , ( TimeStamp {hr = 8, min = 48, sec = 52}
    , Quotation
      "...that fly-fishers fail in preparing their bait so as to make it \nalluring in the right quarter, for want of a due acquaintance with \nthe subjectivity of fishes."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 12, min = 19, sec = 51}
    , Quotation "Lucid and ironic, she knew no merciful muddle." "" Nothing
    )
  , ( TimeStamp {hr = 17, min = 51, sec = 1}
    , Quotation "You misunderstand me. I do not fear death. I _resent_ it."
                ""
                Nothing
    )
  , ( TimeStamp {hr = 17, min = 52, sec = 49}
    , Quotation
      "Miss Morland, no one can think more highly of the understanding of\nwomen than I do. In my opinion, nature has given them so much, that\nthey never find it necessary to use more than half."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 19, min = 31, sec = 2}
    , Quotation
      "...\8212oh, don't go in for accuracy at this house. We all exaggerate, and\nwe get very angry at people who don't."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 20, min = 49, sec = 24}
    , Quotation "...he had shown her the holiness of direct desire." "" Nothing
    )
  , ( TimeStamp {hr = 21, min = 28, sec = 18}
    , Quotation
      "{There's no,What} better antidote to respect than hypocrisy{.,?}"
      "Keane Yahn-Krafft"
      Nothing
    )
  , ( TimeStamp {hr = 21, min = 44, sec = 21}
    , Quotation "Better be without sense, than misapply it as you do."
                ""
                Nothing
    )
  , ( TimeStamp {hr = 23, min = 33, sec = 42}
    , Quotation
      "Rationalization isn't just a river in Egyt\8212wait, that's denial."
      "IZombie"
      Nothing
    )
  ]

globAustenQs =
  [ ( TimeStamp {hr = 17, min = 52, sec = 49}
    , Quotation
      "Miss Morland, no one can think more highly of the understanding of\nwomen than I do. In my opinion, nature has given them so much, that\nthey never find it necessary to use more than half."
      ""
      Nothing
    )
  , ( TimeStamp {hr = 21, min = 44, sec = 21}
    , Quotation "Better be without sense, than misapply it as you do."
                ""
                Nothing
    )
  ]

globAustenEmma =
  [ ( TimeStamp {hr = 21, min = 44, sec = 21}
    , Quotation "Better be without sense, than misapply it as you do."
                ""
                Nothing
    )
  ]

globCommentLexical =
  [ ( TimeStamp {hr = 17, min = 33, sec = 59}
    , Commentary "<Insightful lexical ejaculate /here/>"
    )
  , ( TimeStamp {hr = 17, min = 52, sec = 50}
    , Commentary "<A second jettisoned insight /here/>"
    )
  ]

globMultiPhrQt =
  [ ( TimeStamp {hr = 8, min = 47, sec = 48}
    , Quotation
      "What novelty is worth that sweet monotony where everything is known,\nand _loved_ because it is known?"
      ""
      Nothing
    )
  , ( TimeStamp {hr = 8, min = 48, sec = 52}
    , Quotation
      "...that fly-fishers fail in preparing their bait so as to make it \nalluring in the right quarter, for want of a due acquaintance with \nthe subjectivity of fishes."
      ""
      Nothing
    )
  , (TimeStamp {hr = 8, min = 50, sec = 29}, Def (Defn Nothing ["\"rapt in\""]))
  , ( TimeStamp {hr = 8, min = 51, sec = 44}
    , Def (Defn Nothing ["\"omit to\"", "\"fail of\""])
    )
  , ( TimeStamp {hr = 8, min = 53, sec = 16}
    , Def (Defn Nothing ["\"be hindered of\""])
    )
  ]


globMultiDefVsQt =
  [ ( TimeStamp {hr = 20, min = 49, sec = 24}
    , Quotation "...he had shown her the holiness of direct desire." "" Nothing
    )
  , ( TimeStamp {hr = 19, min = 29, sec = 5}
    , Def
      (DefVersus "putter"
                 "one who puts; to potter"
                 "potter"
                 "one who makes pots; to trifle; to walk lazily "
      )
    )
  ]
