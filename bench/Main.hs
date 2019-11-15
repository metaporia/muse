{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Criterion.Main
import           Data.Foldable                  ( traverse_ )
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sqlite
import           Lib                     hiding ( main )
import           Store.Sqlite            hiding ( main )
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )

main = main'

main'' = defaultMain
  [ bgroup
    "quoteSearch"
    [ bench "noDates" $ nfIO (runQuoteSearchNoDates "unintelligible")
    , bench "withDates" $ nfIO (runQuoteSearchWithDates "unintelligible")
    ]
  , bgroup
    "CLI"
    [ bench "testMain \"search --all -q --qb unintelligible\""
      $ nfIO (testMain "search --all -q --qb unintelligible")
    ]
  ]

runQuoteSearchNoDates :: String -> IO ()
runQuoteSearchNoDates x =
  --runResourceT
  --  . runStdoutLoggingT
  --  . withSqliteConn "/home/aporia/.muse/state/sqlite.db"
  --  . runSqlConn
                          runSqlite "/home/aporia/.muse/state/sqlite.db" $ do
  before   <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since    <- liftIO $ addDays (-12 * 30) . utctDay <$> getCurrentTime
  matching <- filterQuotes'' since before [] [] ["%" <> x <> "%"]
  liftIO $ traverse_ (pPrint . quoteEntryBody . entityVal) matching
  return ()

runQuoteSearchWithDates :: String -> IO ()
runQuoteSearchWithDates x =
    -- runResourceT
    -- . runStdoutLoggingT
    -- . withSqliteConn "/home/aporia/.muse/state/sqlite.db"
    -- . runSqlConn
                            runSqlite "/home/aporia/.muse/state/sqlite.db" $ do
  before   <- liftIO $ addDays 1 . utctDay <$> getCurrentTime
  since    <- liftIO $ addDays (-12 * 30) . utctDay <$> getCurrentTime
  matching <- filterQuotes since before [] [] ["%" <> x <> "%"]
  liftIO $ traverse_ (pPrint . quoteEntryBody . entityVal) matching
  return ()

