{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, TupleSections, QuasiQuotes, FlexibleInstances,
  MultiWayIf #-}
{-# LANGUAGE RecordWildCards, StandaloneDeriving, DeriveDataTypeable,
  TemplateHaskell, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Store
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Uses acid-state and safeCopy to serilaize and persist logs.
--
-- DB' schema:
-----------------------------------------------------------------------------
module Store where

import Control.Exception (bracket)
import Control.Monad ((>=>), void)
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Remote
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), updateIx, ixFun, ixSet, getOne)
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Text.Show.Pretty (pPrint)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)

import Helpers
import Parse (DefQuery(..), PageNum(..), TimeStamp(..))
import Parse.Entry (Entry(..), LogEntry(..), Phrase(..))
import Render (colRender, showAll)
import Time (truncateUTC)
import Store.Types

type IndentDepth = Int
type Quote = Text
type Title = Text
type Author = Text
type Attr = Text
type Body = Text
type PgNum = Int



deriving instance Read TimeStamp
deriving instance Ord TimeStamp
deriving instance Data TimeStamp

deriveSafeCopy 0 'base ''TimeStamp

deriving instance Data     Entry
deriving instance Read     Entry
deriving instance Ord      Entry
deriving instance Typeable Entry

deriveSafeCopy 0 'base ''Entry

deriving instance Data     PageNum
deriving instance Read     PageNum
deriving instance Ord      PageNum
deriving instance Typeable PageNum
deriveSafeCopy 0 'base ''PageNum

deriving instance Data     LogEntry
deriving instance Read     LogEntry
deriving instance Ord      LogEntry
deriving instance Typeable LogEntry
deriveSafeCopy 0 'base ''LogEntry

deriving instance Data     Phrase
deriving instance Read     Phrase
deriving instance Ord      Phrase
deriving instance Typeable Phrase
deriveSafeCopy 0 'base ''Phrase

deriving instance Data     DefQuery
deriving instance Read     DefQuery
deriving instance Ord      DefQuery
deriving instance Typeable DefQuery
deriveSafeCopy 0 'base ''DefQuery

-- | 'LogEntry' with 'Day'.
data LogEntry'
  = Dump' Day
          Text
  | Ts Day
       IndentDepth
       TimeStamp
       Entry
  deriving (Eq, Ord, Read, Show, Data, Typeable)

toLogEntry :: LogEntry' -> (Day, LogEntry)
toLogEntry (Dump' d s) = (d, Dump $ T.unpack s)
toLogEntry (Ts d i ts e) = (d, TabTsEntry (i, ts, e))

pp :: LogEntry' -> IO ()
pp = (\(d, le) -> putStrLn (show d) >> colRender True le) . toLogEntry

data DB' =
  DB' [LogEntry']
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | Since no search predicates target dumps (they're only present for
-- compatibility), they'll be stored in a separate table. This will speed up
-- most searches, barring "fetch everything, including dumps".
--
-- TODO index
data Dumps = Dumps Day (Set Text) -- ^ sorted, unique list of text
  deriving (Eq, Ord, Show, Read, Data)

getDay :: Dumps -> Day
getDay (Dumps d _) = d

getDumps :: Dumps -> Set Text
getDumps (Dumps _ ds) = ds

insertDumpText :: Text -> Dumps -> Dumps
insertDumpText t (Dumps d s) = Dumps d (Set.insert t s)

deriveSafeCopy 0 'base ''Dumps


instance Indexable Dumps where
  empty =
    ixSet
      [ ixFun $ return . getDay -- primary key
      , ixFun $ return . getDumps
      ]


-- | Logs are naturally indexed/bucketed by day, so we won't tamper with that.
-- 
-- TODO index!
data DayLog = DayLog
  { day :: Day
  , entries :: [(Int, TimeStamp, Entry)]
  } deriving (Eq, Ord, Show, Read, Data)


deriveSafeCopy 0 'base ''DayLog

newtype DayLogs = DayLogs [DayLog]
  deriving (Eq, Ord, Show, Read, Data)

getDayLogs (DayLogs dls) = dls

getDays :: DayLogs -> [Day]
getDays = fmap day . getDayLogs

deriveSafeCopy 0 'base ''DayLogs


hasTs :: TimeStamp -> DayLog -> Bool
hasTs ts dl = ts `elem` ((\(_,ts',_) -> ts') <$> entries dl)


x = do
  t <- getCurrentTime
  return ()

-- | Entry variant 'DB' bucket identifier. The original, chronological log
-- order is stored as a set of `(timestamps, tag)` pairs; the tag informs
-- dispatch from which bucket to fetch the assoc'd entry. E.g., if when parsing
-- the 'chrono' set, an entry of the form `(ts, Dmp)` is found, then `ts' will
-- be fetched from `dumped (db :: DB)`.
data Bucket
  = Dmp
  | Rds
  | Qts
  | Dial
  | Phrs
  | Pgs PageNum
  | Cmts
  | Defs
  | Null
  deriving (Eq, Ord, Show)

deriveSafeCopy 0 'base ''Bucket

toBucket :: LogEntry -> Bucket
toBucket (Dump _) = Dmp
toBucket (TabTsEntry (_, _, (Read _ _))) = Rds
toBucket (TabTsEntry (_, _, (Def _))) = Defs
toBucket (TabTsEntry (_, _, (Quotation _ _ _ ))) = Qts
toBucket (TabTsEntry (_, _, (Commentary _))) = Cmts
toBucket (TabTsEntry (_, _, (PN pg))) = Pgs pg
toBucket (TabTsEntry (_, _, (Phr _))) = Phrs
toBucket (TabTsEntry (_, _, (Dialogue _))) = Dial
toBucket (TabTsEntry (_, _, (Parse.Entry.Null))) = Store.Null

-- TODO replace tuples with newtypes, for each, index by utcTime
data DB = DB
  { dumped :: IxSet Dumps
  , defs :: IxSet (TsIdx DefQuery)
  , reads :: IxSet (TsIdxTup Title Author)
  , quotes :: IxSet (TsIdxTup Body Attr)
  , dialogues :: IxSet (TsIdx Text)
  , phrases :: IxSet (TsIdx Phrase)
  , comments :: IxSet (TsIdx Text)
  --, pages :: IxSet (TsIdx PageNum) -- Perhaps keep these in 'chrono'?
  , chrono :: IxSet (TsIdxTup IndentDepth Bucket) -- ^ entry order of elements in entry variant buckets.
  } deriving (Eq, Show)

deriveSafeCopy 0 'base ''DB


initDB :: DB
initDB =
  DB
    IxSet.empty
    IxSet.empty
    IxSet.empty
    IxSet.empty
    IxSet.empty
    IxSet.empty
    IxSet.empty
    IxSet.empty

-- | Updates entry in indexed set of timestamped pairs.
updateIxSetTsIdx ::
     (Ord a, Typeable a) => TsIdx a -> IxSet (TsIdx a) -> IxSet (TsIdx a)
updateIxSetTsIdx ti = updateIx (ts ti) ti


-- | Updates entry in indexed set of `(ts, (a, b))`; that is, pairs of timestamps
-- and tuples.
updateIxSetTsIdxTup ::
     (Ord a, Typeable a, Ord b, Typeable b)
  =>  TsIdxTup a b
  -> IxSet (TsIdxTup a b)
  -> IxSet (TsIdxTup a b)
updateIxSetTsIdxTup tt@(TsIdxTup idx) = updateIx (ts idx) tt



-- | If day is already present, the text is added to that day's dumps,
-- otherwise, a new 'Dumps' is added. Returns the updated or newly created
-- 'Dumps'
insertDump :: Day -> Text -> Update DB Dumps
insertDump day content = do
  db@DB {..} <- get
  let dumps =
        case getOne $ IxSet.getEQ day dumped of
          Just dumps -> insertDumpText content dumps
          Nothing -> Dumps day $ Set.singleton content
  put DB {dumped = updateIx day dumps dumped, ..}
  return dumps

-- | Overwrites '(Title, Author)' tuple.
updateRead :: UTCTime -> Title -> Author -> Update DB (Title, Author)
updateRead ts t a = do
  db@DB {..} <- get
  put DB {reads = updateIxSetTsIdxTup (mkTsIdxTup' ts (t, a)) reads, ..}
  return (t, a)

-- | UTCTimes should be unique (globally), but the values attached to two
-- identetical primary keys may not, and so the 'updateQuote' overwrites the
-- mapping at the given time if it exists.
updateQuote :: UTCTime -> Title -> Author -> Update DB (Title, Author)
updateQuote ts t a = do
  db@DB {..} <- get
  put DB {quotes = updateIxSetTsIdxTup (mkTsIdxTup' ts (t, a)) quotes, ..}
  return (t, a)

updateDialogue :: UTCTime -> Text -> Update DB Text
updateDialogue ts t = do
  db@DB {..} <- get
  put DB {dialogues = updateIxSetTsIdx (mkTsIdx ts t) dialogues, ..}
  return t

updatePhrase :: UTCTime -> Phrase -> Update DB Phrase
updatePhrase ts p = do
  db@DB {..} <- get
  put DB {phrases = updateIxSetTsIdx (mkTsIdx ts p) phrases, ..}
  return p

updateComments :: UTCTime -> Text -> Update DB Text
updateComments ts c = do
  db@DB {..} <- get
  put DB {comments = updateIxSetTsIdx (mkTsIdx ts c) comments, ..}
  return c


viewDB :: Query DB DB
viewDB = ask

reinitDB :: Update DB DB
reinitDB = put initDB >> return initDB

--makeAcidic ''DB' ['addEntry, 'addDump, 'viewDB']
makeAcidic ''DB ['insertDump , 'updateRead , 'updateQuote , 'updateDialogue, 'updatePhrase, 'viewDB, 'reinitDB]

insert :: IO ()
insert = do
  today <- utctDay <$> getCurrentTime
  utc <- getCurrentTime
  utc' <- getCurrentTime
  r <-
    bracket
      (openLocalState initDB)
      createCheckpointAndClose
      (\acid -> do
         update acid (InsertDump today "dump body")
         update acid (UpdateRead utc "Thank You, Jeeves" "P.G. Wodehouse")
         update acid (UpdateRead utc' "Thank You, Jeeves" "P.G. Wodehouse")
         query acid ViewDB
         )
  pPrint r
  return ()

view :: IO ()
view = do
  r <- bracket (openLocalState initDB)
           createCheckpointAndClose
           (\acid -> query acid ViewDB)
  pPrint r

purge :: IO ()
purge = do
  r <- bracket (openLocalState initDB)
           createCheckpointAndClose
           (\acid -> update acid ReinitDB)
  pPrint r


-- TODO
-- □  impl; Q: how to handle key overwrites?
--
-- | Add a day's worth of log entries to DB.
--
updateDB :: Day -> [LogEntry] -> Update DB DB
updateDB  = undefined
