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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)

import Helpers
import Parse (DefQuery(..), PageNum(..), TimeStamp(..))
import Parse.Entry (Entry(..), LogEntry(..), Phrase(..))
import Render (colRender, showAll)

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

deriving instance Data     Entry
deriving instance Read     Entry
deriving instance Ord      Entry
deriving instance Typeable Entry

deriving instance Data     PageNum
deriving instance Read     PageNum
deriving instance Ord      PageNum
deriving instance Typeable PageNum

deriving instance Data     LogEntry
deriving instance Read     LogEntry
deriving instance Ord      LogEntry
deriving instance Typeable LogEntry

deriving instance Data     Phrase
deriving instance Read     Phrase
deriving instance Ord      Phrase
deriving instance Typeable Phrase

deriving instance Data     DefQuery
deriving instance Read     DefQuery
deriving instance Ord      DefQuery
deriving instance Typeable DefQuery

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

-- TODO 
-- □  (index) replace `[DayLog]` with `IxSet DayLog]`
-- □  index by `(Day, TimeStamp)` (primary key?)
-- □  index by `Day`

-- | Datetime map indexed by tuple, fst, snd. fst should be the primary key.
data TsIdx v = TsIdx
  { ts :: UTCTime
  , val :: v
  } deriving (Eq, Ord, Show, Read, Data)

deriveSafeCopy 0 'base ''TsIdx

instance (Ord v, Typeable v) => Indexable (TsIdx v) where
  empty = ixSet [ixFun $ return . ts, ixFun $ return . val]

newtype TsIdxTup a b = TsIdxTup
  { tsIdx :: TsIdx (a, b)
  } deriving (Eq, Ord, Show, Read, Data)

instance (Ord a, Ord b, Typeable a, Typeable b) => Indexable (TsIdxTup a b) where
  empty =
    ixSet
      [ ixFun $ return . ts . tsIdx
      , ixFun $ return . val . tsIdx -- tuple
      , ixFun $ return . fst . val . tsIdx -- fst
      , ixFun $ return . snd . val . tsIdx -- snd
      ]

deriveSafeCopy 0 'base ''TsIdxTup

x = do
  t <- getCurrentTime
  return ()

-- TODO replace tuples with newtypes, for each, index by utcTime
data DB = DB
  { dumped :: IxSet Dumps
  , reads :: IxSet (TsIdxTup Title Author)
  , quotes :: IxSet (TsIdxTup Body Attr)
  , dialogues :: IxSet (TsIdx Text)
  , phrases :: IxSet (TsIdx Phrase)
  , comments :: IxSet (TsIdx Text)
  } deriving (Data, Eq, Ord, Read, Show, Typeable)

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
  put DB {reads = updateIxSetTsIdxTup (TsIdxTup $ TsIdx ts (t, a)) reads, ..}
  return (t, a)

updateQuote :: UTCTime -> Title -> Author -> Update DB (Title, Author)
updateQuote ts t a = do
  db@DB {..} <- get
  put DB {quotes = updateIxSetTsIdxTup (TsIdxTup $ TsIdx ts (t, a)) quotes, ..}
  return (t, a)

updateDialogue :: UTCTime -> Text -> Update DB Text
updateDialogue ts t = do
  db@DB {..} <- get
  put DB {dialogues = updateIxSetTsIdx (TsIdx ts t) dialogues, ..}
  return t

updatePhrase :: UTCTime -> Phrase -> Update DB Phrase
updatePhrase ts p = do
  db@DB {..} <- get
  put DB {phrases = updateIxSetTsIdx (TsIdx ts p) phrases, ..}
  return p

updateComments :: UTCTime -> Text -> Update DB Text
updateComments ts c = do
  db@DB {..} <- get
  put DB {comments = updateIxSetTsIdx (TsIdx ts c) comments, ..}
  return c


updateIxSetTsIdx ::
     (Ord a, Typeable a) => TsIdx a -> IxSet (TsIdx a) -> IxSet (TsIdx a)
updateIxSetTsIdx ti@TsIdx {..} = updateIx ts ti

updateIxSetTsIdxTup ::
     (Ord a, Typeable a, Ord b, Typeable b)
  =>  TsIdxTup a b
  -> IxSet (TsIdxTup a b)
  -> IxSet (TsIdxTup a b)
updateIxSetTsIdxTup tt@(TsIdxTup TsIdx{..}) = updateIx ts tt

update :: Update DB' ()
update = undefined

--insert :: Update DB' ()
--insert = undefined
--
--insert :: Update DB' ()
--insert = undefined
--
--insert :: Update DB' ()
--insert = undefined
--
--insert :: Update DB' ()
--insert = undefined

deriveSafeCopy 0 'base ''DB

initDB' :: DB'
initDB' = DB' []

getDB' :: DB' -> [LogEntry']
getDB' (DB' xs) = xs


deriveSafeCopy 0 'base ''TimeStamp
deriveSafeCopy 0 'base ''Phrase
deriveSafeCopy 0 'base ''PageNum
deriveSafeCopy 0 'base ''DefQuery
deriveSafeCopy 0 'base ''Entry
deriveSafeCopy 0 'base ''LogEntry'
deriveSafeCopy 0 'base ''DB'



-- | Creates 'Ts' from 'Entry'' and inserts it into 'DB'.
addEntry :: Day -> TimeStamp -> Int -> Entry -> Update DB' LogEntry'
addEntry day ts indent entry = do
  db@(DB' entries) <- get
  let entry' = Ts day indent ts entry
  put . DB' $ entry' : entries
  return entry'

-- | Creates 'Entry from 'Text' and inserts it into 'DB'.
addDump :: Day -> Text -> Update DB' LogEntry'
addDump day text = do
  db@(DB' entries) <- get
  let dump = Dump' day text
  put . DB' $ dump : entries
  return dump


viewDB' :: Query DB' [LogEntry']
viewDB' = getDB' <$> ask

makeAcidic ''DB' ['addEntry, 'addDump, 'viewDB']

run :: IO ()
run = do
  let ts = TimeStamp 0 0 0
  today <- utctDay <$> getCurrentTime
  r <-
    bracket
      (openLocalState initDB')
      createCheckpointAndClose
      (\acid -> do
         --update acid (AddDump today "dump body")
         --update acid (AddEntry today ts 0 (Read "Thank You, Jeeves" "P.G. Wodehouse"))
         query acid ViewDB')
  mapM_ pp r
  return ()

--fetch :: IO (0
--fetch = do
--  bracket (openLocalState initDB')


