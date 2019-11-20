{-# LANGUAGE GADTSyntax, GADTs, InstanceSigs, ScopedTypeVariables,
  MultiWayIf, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Search
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module exposes search predicate application functions.
-----------------------------------------------------------------------------
module Search (isTopLevel, isRead, isIndentedTo, pathToDay) where

import           Control.Lens                   ( _Right
                                                , preview
                                                )
import           Control.Lens.Tuple
import           Control.Monad                  ( (>=>)
                                                , join
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Time
import           Helpers
import           Parse
import           Parse.Entry
import           Prelude                 hiding ( log
                                                , lookup
                                                , min
                                                )

-- TODO replace w/ newtype wrapper around `Entry`
data SearchResult
  = Def' String
  | Quotation' Quote
               Author
               (Maybe PgNum)
  | Commentary' Body
  | Read' Title
          Author
  | PN' PageNum
  | Null'
  | Entry' String -- ?
  deriving (Eq, Show)

-- | Parse basename /not/ absolute path into 'Day'.
pathToDay :: FilePath -> Maybe Day
pathToDay = join . preview _Right . showErr . parse day'

-- auto-complete
-- Qs:
--
-- * add "canonical <auth>" entry to log seeds for entity recognition via
--   `rate`?
-- * add entity recog. checking linter option to muse CLI; could be run on save
--   thereby showing errors in (nearly) real-time which allows for on the spot
--   user correction.
--
--   This linter would:
--
--      1. grab line at cursor
--      2. try to extract entry w citation/attribution, a.t.m. only `Read`
--      3. rate against last ~5 authors (take first match); otherwise lazily
--         crawl author history in chunks of size, say, 10
--      4. display (i) the identified author or (ii) that it is unrecognized,
--         with a list of the top, say, 5 matches above the distance threshold.
--
--      tl;dr; try to identify attr under cursor, if successful display result,
--      if not show top matches (above satisfaction threshold)
idea = undefined

-- | Checks whether a 'LogEntry' is indented at least as much as the given
-- depth; for 'Dump's returns @False@ when depth >= 1.
isIndentedTo :: Int -> LogEntry -> Bool
isIndentedTo depth (Dump       _                  ) = depth == 0
isIndentedTo depth (TabTsEntry (indentation, _, _)) = indentation >= depth

-- | Checks whether a 'LogEntry' is indented _exactly_ to the given depth;
-- for 'Dump's returns @False@ unless the given depth is 0.
isIndentedExactlyTo :: Int -> LogEntry -> Bool
isIndentedExactlyTo depth (Dump _) = depth == 0
isIndentedExactlyTo depth (TabTsEntry (indentation, _, _)) =
  indentation == depth

-- | Checks whether a 'LogEntry' is not indented.
-- 'Dumps' are considered to be top-level.
isTopLevel :: LogEntry -> Bool
isTopLevel (TabTsEntry (0, _, _)) = True
isTopLevel (Dump       _        ) = True
isTopLevel _                      = False


isDef :: LogEntry -> Bool
isDef = isJust . projectDefQuery

isRead :: LogEntry -> Bool
isRead = isJust . projectRead

isQuote :: LogEntry -> Bool
isQuote = isJust . projectQuotation

isPhrase :: LogEntry -> Bool
isPhrase = isJust . projectPhrase

isDialogue :: LogEntry -> Bool
isDialogue = isJust . projectDialogue

-- | `LogEntry` projections.
getEntry :: LogEntry -> Maybe Entry
getEntry = preview $ _TabTsEntry . _3

getDump :: LogEntry -> Maybe String
getDump = preview _Dump

-- `Entry` projections
getDefQuery :: Entry -> Maybe DefQuery
getDefQuery = preview _Def

getPageNum :: Entry -> Maybe PageNum
getPageNum = preview _PN

getRead :: Entry -> Maybe (Title, Author)
getRead = preview _Read

getQuotation :: Entry -> Maybe (Quote, Attr, Maybe PgNum)
getQuotation = preview _Quotation

getCommentary :: Entry -> Maybe Body
getCommentary = preview _Commentary

getPhrase :: Entry -> Maybe Phrase
getPhrase = preview _Phr

getDialogue :: Entry -> Maybe String
getDialogue = preview _Dialogue

-- Composed `LogEntry` and `Entry` projections.
projectDefQuery :: LogEntry -> Maybe DefQuery
projectDefQuery = getEntry >=> getDefQuery

projectQuotation = getEntry >=> getQuotation

projectPageNum = getEntry >=> getPageNum

projectRead = getEntry >=> getRead

projectCommentary = getEntry >=> getCommentary

projectPhrase = getEntry >=> getPhrase

projectDialogue = getEntry >=> getDialogue

project :: LogEntry -> Maybe DefQuery
project (TabTsEntry (_, _, Def dq)) = Just dq
project _                             = Nothing

projectDef :: LogEntry -> [(Headword, Maybe Meaning)]
projectDef (TabTsEntry (_, _, Def dq)) = case dq of
  Defn      mPg headwords -> (, Nothing) <$> headwords
  InlineDef hw  meaning   -> [(hw, Just meaning)]
  DefVersus hw m hw' m'   -> [(hw, Just m), (hw', Just m')]
projectDef _ = []

