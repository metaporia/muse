{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  OverloadedStrings, TupleSections, QuasiQuotes, FlexibleInstances,
  MultiWayIf #-}
{-# LANGUAGE RecordWildCards, BangPatterns, NamedFieldPuns,
  StandaloneDeriving, DeriveDataTypeable, TemplateHaskell,
  TypeFamilies #-}

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
--
-- □  finish acid-state based storage/search implementation
--    benchmark against old (expose two @main@s for first round; if it's hard to
--    tell with the UNIX `time` CLI, use a hs benchmarking lib, like "criterion")
--
-- □  egad! timezone protection.
--
-- □  impl; Q: how to handle key overwrites?
--          A: collect overwritten keys (whose values are distinct)
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
import Data.Foldable (foldl')
import Data.IxSet
       (Indexable(..), IxSet(..), Proxy(..), (@<=), (@=), (@>=), (@>=<=),
        getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as IxSet
import Data.Monoid (All(..), Any(..), (<>))
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock (utctDay)
import Helpers
import Parse (DefQuery(..), PageNum(..), TimeStamp(..))
import Parse.Entry (Entry(..), LogEntry(..), Phrase(..))
import Prelude
import Render (colRender, showAll)
import Search
import Store.Types
import Text.Show.Pretty (pPrint)
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

deriving instance Data Entry

deriving instance Read Entry

deriving instance Ord Entry

deriving instance Typeable Entry

deriveSafeCopy 0 'base ''Entry

deriving instance Data LogEntry

deriving instance Read LogEntry

deriving instance Ord LogEntry

deriving instance Typeable LogEntry

deriveSafeCopy 0 'base ''LogEntry

deriving instance Data Phrase

deriving instance Read Phrase

deriving instance Ord Phrase

deriving instance Typeable Phrase

deriveSafeCopy 0 'base ''Phrase

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
data Dumps =
  Dumps Day
        (Set Text) -- ^ sorted, unique list of text
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

newtype DayLogs =
  DayLogs [DayLog]
  deriving (Eq, Ord, Show, Read, Data)

getDayLogs (DayLogs dls) = dls

getDays :: DayLogs -> [Day]
getDays = fmap day . getDayLogs

deriveSafeCopy 0 'base ''DayLogs

hasTs :: TimeStamp -> DayLog -> Bool
hasTs ts dl = ts `elem` ((\(_, ts', _) -> ts') <$> entries dl)

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
  , quotes :: IxSet (TsIdxTag (Body, Attr, (Maybe PgNum)))
  , dialogues :: IxSet (TsIdx Text)
  , phrases :: IxSet (TsIdxTag Phrase)
  , comments :: IxSet (TsIdxTag Text)
  -- | entry order of elements in entry variant buckets.
  , chrono :: IxSet (TsIdxTup IndentDepth Bucket)
  -- | Record when a day's entries were last updated.
  , lastUpdated :: IxSet ModRec
  } deriving (Eq, Show)

data ModRec = ModRec { date :: Day, modified :: UTCTime }
  deriving (Eq, Ord, Show, Read, Data)

instance Indexable ModRec where
  empty = ixSet [ ixFun $ return . date -- primary key
                , ixFun $ return . modified -- UTCTime
                ]

deriveSafeCopy 0 'base ''ModRec

deriveSafeCopy 0 'base ''DB

-- | Type returned by 'DB' queries. Nearly identical to 'LogEntry'.
--
-- TODO
-- □  'ColRender' instance
-- □  'fromLogEntry :: LogEnty -> Result'
-- □  'fromEntry :: Enty -> Result'
data Result
  = DumpR Text
  | TsR UTCTime
        Entry -- ^ N.B. doesn't use 'Entry' variant 'PN' 
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

newtype Results = Results
  { results :: [Result]
  } deriving (Eq, Show)

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
                    (T.unpack . getFst . val $ tsTag i)
                    (T.unpack . getSnd . val $ tsTag i)
                    (getThrd . val $ tsTag i)) :
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

-- ** 'DB' management
-- | Insert 'LogEntry' into 'DB'. For each, an entry is added to 'chrono' and,
-- except for 'Null' and 'Phrs', to the appropriate bucket. Adds 'AttrTag'
-- when applicable (see definition of 'DB').
--
-- WARNING: this function is solely responsible for guaranteeing UTCTime
-- truncation. Should this be done inconsistently, lookups will begin to fail
-- silently. Take, for instance, the logic of 'readSatisfies' which would
-- reject potentially satisfactory entries on account of a timestamp precision
-- mismatch.
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
      in updateDef utc dq tag >> updateChrono utc indent Defs >>
         return (Just utc)
    (TabTsEntry (indent, ts, (Quotation q attr mp))) ->
      let utc = (toUTC day ts)
      in updateQuote utc (T.pack q) (T.pack attr) (fmap fromIntegral mp) tag >>
         updateChrono utc indent Qts >>
         return (Just utc)
    (TabTsEntry (indent, ts, (Commentary body))) ->
      let utc = (toUTC day ts)
      in updateComment utc (T.pack body) tag >> updateChrono utc indent Cmts >>
         return (Just utc)
    (TabTsEntry (indent, ts, (PN pg))) ->
      let utc = (toUTC day ts)
      in updateChrono utc indent (Pgs pg) >> return (Just utc)
    (TabTsEntry (indent, ts, (Phr p))) ->
      let utc = (toUTC day ts)
      in updatePhrase utc p tag >> updateChrono utc indent Phrs >>
         return (Just utc)
    (TabTsEntry (indent, ts, (Dialogue body))) ->
      let utc = (toUTC day ts)
      in updateDialogue utc (T.pack body) >> updateChrono utc indent Dial >>
         return (Just utc)
    (TabTsEntry (indent, ts, (Parse.Entry.Null))) -> return Nothing

-- | Tags and adds to 'DB' a day's worth of 'LogEntry's. Logs time of
-- invocation in 'lastUpdated' to aid in caching parsed and unchanged logs.
--
-- TODO test!!
addDay :: Day -> UTCTime -> [LogEntry] -> Update DB ()
addDay day utc le =
  if not (null le)
    then updateLastUpdated day utc >> tagAndUpdate le
    else return ()
    --naive = void . sequence . fmap (\le -> addLogEntry day le Nothing) $ les
  where
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
    tagRest tag = foldr f (return []) -- note use of @foldl'@ for strictness
      where
        f x acc
          | isIndentedTo 1 x =
            acc >>= \les -> (addLogEntry day x tag >> return les)
          | otherwise = (x :) <$> acc

data Attribution =
  Attribution Title
              Author
  deriving (Eq, Show)

-- | Lists of predicates applied and folded with @All@.
data Search' =
  Search' [Attribution -> Bool]
          VariantSearch'

-- Broke AF
data VariantSearch'
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

data Search where
  Search
    :: Day -- ^ Start date
    -> Day -- ^ End date
    -> [Attribution -> Bool] -- ^ title/auth preds
    -> BucketList -- ^ bucket specific predicates
    -> Search

bucketList (Search _ _ _ bl) = bl

-- N.B. apply attribution filters to all but dialogues.
data BucketList = BucketList
  { dumpsPreds :: [Text -> Bool]
  , defsPreds :: ([Headword -> Bool], [Meaning -> Bool])
  , readsPreds :: [Attribution -> Bool]
  , quotesPreds :: [Text -> Bool]
  , dialoguesPreds :: [Text -> Bool]
  , phrasesPreds :: ([Headword -> Bool], [Meaning -> Bool])
  , commentsPreds :: [Text -> Bool]
  }

-- | Empty 'BucketList'.
initBucketList :: BucketList
initBucketList = BucketList [] ([], []) [] [] [] ([], []) []

-- | Dispatches search predicates.
--
-- Search logic:
--
-- 1. check whether list of title/auth preds is empty; if so, do not
--    dereference tags and proceed with bucketlist-based filtration
--  i. check whether there all members of bucketlist are None or all are Maybe;
--     if so, apply filters to chrono--this will save us time sorting filtered
--     buckets--, otherwise, fetch entries from each requested bucket within
--     date range with @entries @>= s @=< e@ and package it up in a 'DB' 
--
--     (use some @collectHeads :: DB -> Maybe [Attribution -> Bool] -> Maybe AttrCache 
--     -> BucketList -> [Results]@ that takes the
--     first entry that satisfies from each non-empty bucket (unwanted buckets should 
--     be empty at this point) and sorts them; repeat until all buckets are
--     empty, and/or 'collectHeads' returns an empty set.)
--
--  ii. should title/auth preds be present, create a @AttrCache :: [(AttrTag ->
--      Attribution)] -> AttrCache@ and then apply 'collectHeads'
--
-- OR, more simply, favor single variant searchs like so:
--
--  (USE ME!) for each bucket type requested, dispatch relevant predicates
--   to it. return the sorted union of the results. BOOYAH
--
--   N.B. Since (I think) the most common/useful search queries will have a
--   specific result in mind (which implies a single desired return type), to
--   optimize single type lookup will benefit users (or at least me) more than
--   crippling fast single entry variant filtration in order to speed up
--   joining the results of filtered lists. In any case, the first pass will
--   use this latter implementation sketch.
--
--   The body of the search function will be a case expression on the search
--   object, which should contain a field for each bucket containing
--   bucket-specific predicates, and a general field, say, for auth/title
--   preds, to be applied as appropriate. The current 'Search' type seems to
--   suffice.
filterBuckets :: Query DB Results
filterBuckets = undefined

-- | Applies auth/title preds, and dump searches.
filterDumps :: Search -> IxSet Dumps -> [Result]
filterDumps (Search s e _ BucketList {dumpsPreds}) ixSet =
  case dumpsPreds of
    [] -> DumpR <$> dumped'
    xs -> DumpR <$> filter satisfiesAll dumped'
  where
    dumped' =
      (IxSet.toAscList (Proxy :: Proxy Day) $ ixSet @>=<= (s, e)) >>=
      Set.toList . getDumps
    -- TODO add pred for fold over boolean OR, that is, @Any@
    satisfiesAll t = foldl' (&&) True $ dumpsPreds <*> [t]

filterDefs :: DB -> Search -> IxSet (TsIdxTag DefQuery) -> [Result]
filterDefs db (Search s e authPreds BucketList {defsPreds}) defs
  | null authPreds =
    case defsPreds of
      ([], []) -> (\t -> TsR (ts . tsTag $ t) (Def . val . tsTag $ t)) <$> defs'
      (hs, ms) ->
        filtermap
          (\t ->
             let dq = val $ tsTag t
             in if (satisfiesAll hs ms dq)
                  then Just $ TsR (ts . tsTag $ t) (Def dq)
                  else Nothing)
          defs'
  | otherwise =
    let (hs, ms) = defsPreds
    in filtermap
         (\tag ->
            let dq = val $ tsTag tag
            in if satisfiesAll hs ms dq && readSatisfies s e db tag authPreds
                 then Just $ TsR (ts . tsTag $ tag) (Def dq)
                 else Nothing)
         defs'
  where
    defs'' = val . tsTag <$> defs'
    defs' = IxSet.toAscList (Proxy :: Proxy Day) $ defs @>=<= (s, e)
    satisfiesAll [] [] _ = True -- allow all without any preds
    satisfiesAll [] _ (Defn _ hws) = False
    satisfiesAll hs _ (Defn _ hws) = foldl' (&&) True (hs <*> hws)
    satisfiesAll hs ms (InlineDef hw mn) =
      foldl' (&&) True (hs <*> pure hw) && foldl' (&&) True (ms <*> pure mn)
    satisfiesAll hs ms (DefVersus hw mn hw' mn') =
      satisfiesAll hs ms (InlineDef hw mn) || -- FIXME or for 'DefVersus'
      satisfiesAll hs ms (InlineDef hw' mn')

-- | Applies title and author predicates to 'AttrTag' assoc'd with given 'DB'
-- entry.
--
-- From tagged 'DB' value fetches assoc'd 'Read' entry to which it applies auth/title
-- predicates.
--
-- Defaults to 'True', as the result is (usuall) ANDed with the results of the
-- locally defined `satisfiesAll`.
--
-- However, if there are attributions but some attribution predicates present,
-- then it returns 'False' as we shouldn't want to contaminate the output.
readSatisfies :: Day -> Day -> DB -> TsIdxTag a -> [Attribution -> Bool] -> Bool
-- no auth/title preds, allows all entries to pass 
readSatisfies _ _ _ _ [] = True
-- no 'AttrTag' present, 
readSatisfies _ _ _ (TsIdxTag tsTag Nothing) _ = False
readSatisfies s e DB {reads} (TsIdxTag tsTag (Just attr)) ps
  -- FIXME: should the attribution of an entry be outside of the query's date
  -- range, this will return false. Luckily, as yet this is impossible, since
  -- there's no way for attributions to span days due to the log file's being
  -- grouped by day.
 =
  case fmap (val . tsIdx) . getOne $ reads @>=<= (s, e) @= (attrId attr) of
    Just (t, a) -> foldl' (&&) True $ ps <*> pure (Attribution t a)
    Nothing -> False

filterDialogues :: DB -> Search -> IxSet (TsIdx Text) -> [Result]
filterDialogues db (Search s e _ BucketList {dialoguesPreds}) ds =
  case dialoguesPreds of
    [] -> (\t -> TsR (ts t) $ Dialogue . T.unpack . val $ t) <$> ds'
    _ ->
      filtermap
        (\t ->
           let d = val t
           in if satisfiesAll d
                then Just $ TsR (ts t) $ Dialogue . T.unpack . val $ t
                else Nothing)
        ds'
  where
    ds' = (IxSet.toAscList (Proxy :: Proxy Day) $ ds @>=<= (s, e))
    -- TODO add pred for fold over boolean OR, that is, @Any@
    satisfiesAll t = foldl' (&&) True $ dialoguesPreds <*> [t]

filterPhrases :: DB -> Search -> IxSet (TsIdxTag Phrase) -> [Result]
filterPhrases db (Search s e authPreds BucketList {phrasesPreds}) phrases
  | null authPreds =
    case phrasesPreds of
      ([], []) -> (\t -> TsR (ts $ tsTag t) (Phr . val $ tsTag t)) <$> phrases'
      (hs, ms) ->
        filtermap
          (\t ->
             let p = val . tsTag $ t
             in if satisfiesAll hs ms (val . tsTag $ t)
                  then Just $ TsR (ts . tsTag $ t) (Phr p)
                  else Nothing)
          phrases'
  | otherwise =
    case phrasesPreds of
      ([], []) ->
        filtermap
          (\t ->
             if readSatisfies s e db t authPreds
               then Just $ TsR (ts . tsTag $ t) (Phr . val $ tsTag t)
               else Nothing)
          phrases'
      (hs, ms) ->
        filtermap
          (\tag ->
             let p = val $ tsTag tag
             in if satisfiesAll hs ms p && readSatisfies s e db tag authPreds
                  then Just $ TsR (ts $ tsTag tag) (Phr p)
                  else Nothing)
          phrases'
  where
    phrases'' = val . tsTag <$> phrases'
    phrases' = IxSet.toAscList (Proxy :: Proxy Day) $ phrases @>=<= (s, e)
    satisfiesAll [] [] _ = True
    satisfiesAll [] _ (Plural phrs) = False
    satisfiesAll hs _ (Plural phrs) = foldl' (&&) True (hs <*> phrs)
    satisfiesAll hs ms (Defined hw mn) =
      foldl' (&&) True (hs <*> pure hw) && foldl' (&&) True (ms <*> pure mn)

-- FIXME text vs string
filterComments :: DB -> Search -> IxSet (TsIdxTag Text) -> [Result]
filterComments db (Search s e authPreds BucketList {commentsPreds}) cmts
  | null authPreds =
    case commentsPreds of
      [] ->
        fmap
          (\t -> TsR (ts $ tsTag t) (Commentary . T.unpack . val . tsTag $ t))
          cmts'
      _ ->
        filtermap
          (\t ->
             let c = val $ tsTag t
             in if satisfiesAll commentsPreds c
                  then Just $ TsR (ts $ tsTag t) (Commentary $ T.unpack c)
                  else Nothing)
          cmts'
  | otherwise =
    filtermap
      (\tag ->
         let c = val $ tsTag tag
         in if satisfiesAll commentsPreds c &&
               readSatisfies s e db tag authPreds
              then Just $ TsR (ts $ tsTag tag) $ Commentary $ T.unpack c
              else Nothing)
      cmts'
  where
    cmts'' = val . tsTag <$> cmts'
    cmts' = IxSet.toAscList (Proxy :: Proxy Day) $ cmts @>=<= (s, e)
    satisfiesAll [] t = True
    satisfiesAll preds t = foldl' (&&) True $ preds <*> [t]

-- TODO rewrite !!!!!
-- Q: filter by manual attributions, or nesting?
-- A: filter by tag if present, fallback to manual attribution
filterQuotes ::
     DB
  -> Search
  -> IxSet (TsIdxTag (Body, Attr, Maybe PgNum))
  -> [Result]
filterQuotes db (Search s e authPreds BucketList {quotesPreds}) quotes
  | null authPreds =
    case quotesPreds of
      [] -> fmap (\t -> let (b, attr, mPg) = val $ tsTag t
                         in TsR (ts $ tsTag t) (Quotation (T.unpack b) (T.unpack attr) mPg))
                   quotes'
      _ ->
        filtermap 
          (\t -> 
            let (b, attr, mPg) = val $ tsTag t
             in if foldl' (&&) True $ quotesPreds <*> [b]
                   then Just $ TsR (ts $ tsTag t) (Quotation (T.unpack b) (T.unpack attr) mPg)
                   else Nothing) 
                   quotes'
        --filter (\(b, _, _) -> foldl' (&&) True $ quotesPreds <*> [b]) quotes''
  | otherwise =
    filtermap
      (\t ->
        let (b, attr, mPg) = val $ tsTag t
         in if satisfiesAll quotesPreds (val $ tsTag t) && readSatisfies' t
               then Just $ TsR (ts $ tsTag t) (Quotation (T.unpack b) (T.unpack attr) mPg)
               else Nothing)
               quotes'
  where
    quotes'' = val . tsTag <$> quotes'
    quotes' = IxSet.toAscList (Proxy :: Proxy Day) $ quotes @>=<= (s, e)
    satisfiesAll :: [Text -> Bool] -> (Body, Attr, Maybe PgNum) -> Bool
    satisfiesAll preds (b, _, _) = foldl' (&&) True (preds <*> [b])
    readSatisfies' :: TsIdxTag (Body, Attr, Maybe PgNum) -> Bool
    readSatisfies' tag =
      case attr tag of
        Just (attrTag) -> readSatisfies s e db tag authPreds
        -- no tag
        Nothing ->
          let (_, a, _) = val . tsTag $ tag
          -- FIXME puts whole attr in both title and auth slot.
          in foldl' (&&) True (authPreds <*> [Attribution a a])

-- TODO cache attributions
filterReads :: Search -> Query DB Results
filterReads (Search s e authPreds BucketList {readsPreds}) = do
  DB {reads} <- ask
  undefined

getPredFor Dmp = undefined
getPredFor Rds = undefined
getPredFor Qts = undefined
getPredFor Dial = undefined
getPredFor Phrs = undefined
getPredFor (Pgs pg) = undefined
getPredFor Cmts = undefined
getPredFor Defs = undefined
getPredFor Store.Types.Null = undefined

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
    IxSet.empty

-- | Updates entry in indexed set of timestamped pairs.
updateIxSetTsIdx ::
     (Ord a, Typeable a) => TsIdx a -> IxSet (TsIdx a) -> IxSet (TsIdx a)
updateIxSetTsIdx ti = updateIx (ts ti) ti

-- | Updates entry in indexed set of timestamped pairs.
updateIxSetTsIdxTag ::
     (Ord a, Typeable a)
  => TsIdxTag a
  -> IxSet (TsIdxTag a)
  -> IxSet (TsIdxTag a)
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
  -> Maybe AttrTag
  -> Update DB (Text, Text, Maybe PgNum)
updateQuote ts b a mp tag = do
  db@DB {..} <- get
  put
    DB {quotes = updateIxSetTsIdxTag (mkTsIdxTag ts (b, a, mp) tag) quotes, ..}
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

updateLastUpdated :: Day -> UTCTime -> Update DB ()
updateLastUpdated d ts = do
  DB {..} <- get
  put DB {lastUpdated = updateIx d (ModRec d ts) lastUpdated, ..}
  return ()

viewDB :: Query DB DB
viewDB = ask

viewLastUpdated :: Query DB (IxSet ModRec)
viewLastUpdated = lastUpdated <$> ask

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
  , 'updateComment
  , 'viewDB
  , 'reinitDB
  , 'addLogEntry
  , 'addDay
  , 'fromDB
  , 'viewLastUpdated
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
      (\acid
         --update acid (InsertDump today "dump body")
         --update acid (UpdateRead utc "Thank You, Jeeves" "P.G. Wodehouse")
         --update acid (UpdateRead utc' "Thank You, Jeeves" "P.G. Wodehouse")
        -> do
         update acid (UpdateDef utc' (Defn Nothing ["concupiscent"]) Nothing)
         query acid ViewDB)
  pPrint r
  return ()

view :: IO ()
view = do
  r <-
    bracket
      (openLocalStateFrom "state/DB" initDB)
      createCheckpointAndClose
      (\acid -> query acid ViewDB >>= pPrint)
  pPrint r

purge :: IO ()
purge = do
  r <-
    bracket
      (openLocalState initDB)
      createCheckpointAndClose
      (\acid -> update acid ReinitDB)
  pPrint r


