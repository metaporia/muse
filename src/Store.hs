{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, TupleSections, QuasiQuotes, FlexibleInstances,
  MultiWayIf #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, StandaloneDeriving,
  DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

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
-- TODO
-- □  impl; Q: how to handle key overwrites?
--          A: collect overwritten keys (whose values are distinct)
-----------------------------------------------------------------------------
module Store where

import Prelude
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
import Data.IxSet
       (Indexable(..), IxSet(..), Proxy(..), (@=), getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as IxSet
import Data.Monoid ((<>))
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Text.Show.Pretty (pPrint)

import Helpers
import Parse (DefQuery(..), PageNum(..), TimeStamp(..))
import Parse.Entry (Entry(..), LogEntry(..), Phrase(..))
import Search
import Render (colRender, showAll)
import Store.Types
import Time (toUTC, truncateUTC)

type IndentDepth = Int
type Quote = Text
type Title = Text
type Author = Text
type Attr = Text
type Body = Text
type PgNum = Integer

deriving instance Read TimeStamp
deriving instance Ord TimeStamp
deriving instance Data TimeStamp

deriveSafeCopy 0 'base ''TimeStamp

deriving instance Data     Entry
deriving instance Read     Entry
deriving instance Ord      Entry
deriving instance Typeable Entry

deriveSafeCopy 0 'base ''Entry

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

-- TODO replace tuples with newtypes
data DB = DB
  { dumped :: IxSet Dumps
  , defs :: IxSet (TsIdxTag DefQuery)
  , reads :: IxSet (TsIdxTup Title Author)
  -- TODO fix tagging logic, that is, optional manual attribution; blocked on
  -- parser update.
  , quotes :: IxSet (TsIdxTrip Body Attr (Maybe PgNum))
  , dialogues :: IxSet (TsIdx Text)
  , phrases :: IxSet (TsIdxTag Phrase)
  , comments :: IxSet (TsIdxTag Text)
  -- | entry order of elements in entry variant buckets.
  , chrono :: IxSet (TsIdxTup IndentDepth Bucket) 
  } deriving (Eq, Show)

deriveSafeCopy 0 'base ''DB

-- ** Search logic

data Attribution = Attribution Title Author
  deriving (Eq, Show)

data Search = Attr [Attribution -> Bool] VariantSearch
            | Other [Text -> Bool] -- ^ a.t.m., only for 'Dialogue'

data VariantSearch
  = NullS -- ^ for, e.g., 'Read', where 'Search' exhausts predicates
  -- | search quote body; may extend to multiple body preds
  | QuoteS [Text -> Bool]
  | DefS [Headword -> Bool] -- FIXME: return matching headwords or entire `[Headword]`?
         [Meaning -> Bool] -- ^ only applied to 'InlineDef's
  | PhrS [Headword -> Bool]
         [Meaning -> Bool] -- ^ only applied to 'Defined's
  | DialogueS [Text -> Bool] -- ^ infix search of comment body
  | CommentS [Text -> Bool]
  | DumpS [Text -> Bool]


-- | Type returned by 'DB' queries. Nearly identical to 'LogEntry'.
--
-- TODO
-- □  'ColRender' instance
-- □  'fromLogEntry :: LogEnty -> Result'
-- □  'fromEntry :: Enty -> Result'
data Result = DumpR Text
            | TsR UTCTime Entry -- ^ N.B. doesn't use 'Entry' variant 'PN' 
            deriving (Eq, Show)

deriveSafeCopy 0 'base ''Result

getUTC :: Result -> Maybe UTCTime
getUTC (DumpR _) = Nothing
getUTC (TsR utc _) = Just utc

fromLogEntry :: Day -> LogEntry -> Result
fromLogEntry _ (Dump s) = DumpR (T.pack s)
fromLogEntry day (TabTsEntry (_, ts, e)) = fromEntry (toUTC day ts) e

fromEntry :: UTCTime -> Entry -> Result
fromEntry utc entry = TsR utc entry

fromDumps :: Dumps -> [Result]
fromDumps (Dumps _ set) = DumpR <$> (Set.toList set)

newtype Results = Results { results :: [Result] }
  deriving (Eq, Show)

deriveSafeCopy 0 'base ''Results


-- Traverses @chrono@ and rebuilds input.
fromDB' :: DB -> Results
fromDB' DB {chrono, ..} =
  Results . go $ IxSet.toAscList (Proxy :: Proxy UTCTime) chrono
  where
    go = foldr f []
    f (TsIdxTup tsIdx) xs =
      let t = ts tsIdx
      in case snd . val $ tsIdx of
           Pgs pg -> fromEntry t (PN pg) : xs
           Dmp ->
             case getOne $ dumped @= t of
               Just dumps -> fromDumps dumps
               Nothing -> xs
           Qts ->
             case getOne $ quotes @= t of
               Just i ->
                 (fromEntry t $
                  Quotation
                    (T.unpack $ getFst i)
                    (T.unpack $ getSnd i)
                    (getThrd i)) :
                 xs
               Nothing -> xs
           Rds ->
             case getOne $ reads @= t of
               Just (TsIdxTup i) ->
                 (fromEntry t $
                  Read (T.unpack . fst . val $ i) (T.unpack . snd . val $ i)) :
                 xs
               Nothing -> xs
           Dial ->
             case getOne $ dialogues @= t of
               Just idx -> (fromEntry t . Dialogue . T.unpack . val $ idx) : xs
               Nothing -> xs
           Defs ->
             case getOne $ defs @= t of
               Just idx -> (fromEntry t . Def . val . tsTag $ idx) : xs
               Nothing -> xs
           Phrs ->
             case getOne $ phrases @= t of
               Just idx -> (fromEntry t . Phr . val . tsTag $ idx) : xs
               Nothing -> xs
           Cmts ->
             case getOne $ comments @= t of
               Just idx ->
                 (fromEntry t . Commentary . T.unpack . val . tsTag $ idx) : xs
               Nothing -> xs
           Store.Types.Null -> xs
fromDB :: Query DB Results
fromDB = fromDB' <$> ask


-- | Applies search predicates to 'DB'.
search :: Query DB [Result]
search = undefined


-- ** 'DB' management

-- | Insert 'LogEntry' into 'DB'. For each, an entry is added to 'chrono' and,
-- except for 'Null' and 'Phrs', to the appropriate bucket. Adds 'AttrTag'
-- when applicable (see definition of 'DB').
addLogEntry :: Day -> LogEntry -> Maybe AttrTag -> Update DB (Maybe UTCTime)
addLogEntry day le tag = do
  db@DB {..} <- get
  case le of
    (Dump body) -> insertDump day (T.pack body) >> return Nothing -- FIXME dumps have no 'chrono' entry?
    (TabTsEntry (indent, ts, (Read t a))) ->
      let utc = (toUTC day ts)
      in updateRead utc (T.pack t) (T.pack a) >> updateChrono utc indent Rds >>
         return (Just utc)
    (TabTsEntry (indent, ts, (Def dq))) ->
      let utc = (toUTC day ts)
      in updateDef utc dq tag >> updateChrono utc indent Defs
         >> return (Just utc)
    (TabTsEntry (indent, ts, (Quotation q attr mp))) ->
      let utc = (toUTC day ts)
      in updateQuote utc (T.pack q) (T.pack attr) (fmap fromIntegral mp) >>
         updateChrono utc indent Qts
         >> return (Just utc)
    (TabTsEntry (indent, ts, (Commentary body))) ->
      let utc = (toUTC day ts)
      in updateComment utc (T.pack body) tag >> updateChrono utc indent Cmts
         >> return (Just utc)
    (TabTsEntry (indent, ts, (PN pg))) ->
      let utc = (toUTC day ts)
      in updateChrono utc indent (Pgs pg)
         >> return (Just utc)
    (TabTsEntry (indent, ts, (Phr p))) ->
      let utc = (toUTC day ts)
      in updatePhrase utc p tag >> updateChrono utc indent Phrs
         >> return (Just utc)
    (TabTsEntry (indent, ts, (Dialogue body))) ->
      let utc = (toUTC day ts)
      in updateDialogue utc (T.pack body) >> updateChrono utc indent Dial
         >> return (Just utc)
    (TabTsEntry (indent, ts, (Parse.Entry.Null))) -> return Nothing

-- | Tags and adds to 'DB' a day's worth of 'LogEntry's.
--
-- TODO test!!
addDay :: Day -> [LogEntry] -> Update DB ()
addDay day = tagAndUpdate
  where
    --naive = void . sequence . fmap (\le -> addLogEntry day le Nothing) $ les
    tagAndUpdate :: [LogEntry] -> Update DB ()
    tagAndUpdate [] = return ()
    tagAndUpdate (x:xs)
      -- only tags when read entry is toplevel
      | isRead x && isIndentedTo 0 x =
        addLogEntry day x Nothing >>= (\t -> tagRest (AttrTag <$> t) xs) >>=
        tagAndUpdate -- tag and update rest of indented; recurse
      | otherwise = addLogEntry day x Nothing >> tagAndUpdate xs
    -- | Given id of parent read entry, @tagRest@ inserts tagged entries
    -- as long as their indentation level never drops below 1. Returns
    -- untagged. 
    -- FIXME take @AttrTag@ not @Maybe AttrTag@. @tagRest@ should not be
    -- invoked with @Nothing@.
    tagRest :: Maybe AttrTag -> [LogEntry] -> Update DB [LogEntry] -- rest
    tagRest tag = go tag $ return [] -- ((return []) :: Update DB [LogEntry])
      where go :: Maybe AttrTag -> Update DB [LogEntry] -> [LogEntry] -> Update DB [LogEntry]
            go tag acc [] = acc
            go tag acc (x:xs) 
              | isIndentedTo 1 x = go tag (acc >> addLogEntry day x tag >> return []) xs 
              | otherwise = return (x : xs)

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

-- | Updates entry in indexed set of timestamped pairs.
updateIxSetTsIdxTag ::
     (Ord a, Typeable a) => TsIdxTag a -> IxSet (TsIdxTag a) -> IxSet (TsIdxTag a)
updateIxSetTsIdxTag tt@(TsIdxTag idx attr) = updateIx (ts idx) tt


-- | Updates entry in indexed set of `(ts, (a, b))`; that is, pairs of timestamps
-- and tuples.
updateIxSetTsIdxTup ::
     (Ord a, Typeable a, Ord b, Typeable b)
  => TsIdxTup a b
  -> IxSet (TsIdxTup a b)
  -> IxSet (TsIdxTup a b)
updateIxSetTsIdxTup tt@(TsIdxTup idx) = updateIx (ts idx) tt

updateIxSetTsIdxTrip ::
     (Ord a, Typeable a, Ord b, Typeable b, Ord c, Typeable c)
  => TsIdxTrip a b c
  -> IxSet (TsIdxTrip a b c)
  -> IxSet (TsIdxTrip a b c)
updateIxSetTsIdxTrip tt@(TsIdxTrip idx) = updateIx (ts idx) tt

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

-- | Overwrites '(Title, Author)' tuple.
updateDef :: UTCTime -> DefQuery -> Maybe AttrTag -> Update DB DefQuery
updateDef ts dq tag = do
  db@DB {..} <- get
  put DB {defs = updateIxSetTsIdxTag (mkTsIdxTag ts dq tag) defs, ..}
  return dq

-- | UTCTimes should be unique (globally), but the values attached to two
-- identetical primary keys may not, and so the 'updateQuote' overwrites the
-- mapping at the given time if it exists.
updateQuote ::
     UTCTime
  -> Text
  -> Text
  -> Maybe PgNum
  -> Update DB (Text, Text, Maybe PgNum)
updateQuote ts b a mp = do
  db@DB {..} <- get
  put DB {quotes = updateIxSetTsIdxTrip (mkTsIdxTrip' ts (b, a, mp)) quotes, ..}
  return (b, a, mp)

updateDialogue :: UTCTime -> Text -> Update DB Text
updateDialogue ts t = do
  db@DB {..} <- get
  put DB {dialogues = updateIxSetTsIdx (mkTsIdx ts t) dialogues, ..}
  return t

updatePhrase :: UTCTime -> Phrase -> Maybe AttrTag -> Update DB Phrase
updatePhrase ts p tag = do
  db@DB {..} <- get
  put DB {phrases = updateIxSetTsIdxTag (mkTsIdxTag ts p tag) phrases, ..}
  return p

updateComment :: UTCTime -> Text -> Maybe AttrTag -> Update DB Text
updateComment ts c tag = do
  db@DB {..} <- get
  put DB {comments = updateIxSetTsIdxTag (mkTsIdxTag ts c tag) comments, ..}
  return c

updateChrono ::
     UTCTime -> IndentDepth -> Bucket -> Update DB (IndentDepth, Bucket)
updateChrono ts i b = do
  db@DB {..} <- get
  put DB {chrono = updateIxSetTsIdxTup (mkTsIdxTup' ts (i, b)) chrono, ..}
  return (i, b)

viewDB :: Query DB DB
viewDB = ask

reinitDB :: Update DB DB
reinitDB = put initDB >> return initDB

makeAcidic
  ''DB
  [ 'insertDump
  , 'updateDef
  , 'updateRead
  , 'updateQuote
  , 'updateDialogue
  , 'updatePhrase
  , 'viewDB
  , 'reinitDB
  , 'addLogEntry
  , 'addDay
  , 'fromDB
  ]

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
         --update acid (InsertDump today "dump body")
         --update acid (UpdateRead utc "Thank You, Jeeves" "P.G. Wodehouse")
         --update acid (UpdateRead utc' "Thank You, Jeeves" "P.G. Wodehouse")
         update acid (UpdateDef utc' (Defn Nothing ["concupiscent"]) Nothing)
         query acid ViewDB)
  pPrint r
  return ()

view :: IO ()
view = do
  r <-
    bracket
      (openLocalState initDB)
      createCheckpointAndClose
      (\acid -> query acid ViewDB)
  pPrint r

purge :: IO ()
purge = do
  r <-
    bracket
      (openLocalState initDB)
      createCheckpointAndClose
      (\acid -> update acid ReinitDB)
  pPrint r
