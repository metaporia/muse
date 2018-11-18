{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  SearchSpec
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Tests search predicate application.
-----------------------------------------------------------------------------
module SearchSpec where

import Data.List (isInfixOf)
import Data.Time
import Helpers
import Parse
import Parse.Entry
import Prelude hiding (min)
import Render
import Search
import Test.Hspec
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)
import Text.Trifecta
import qualified Text.Trifecta.Result as Tri

-- `--author` and `--quotations` flags disable (some) filtration

showTest ap tp preds = test' ap tp preds >>= showAll

test' ap tp preds = flip filterWith' sample <$> input ap tp preds
test ap tp preds = flip filterWith' tDialogueFilter <$> input ap tp preds

--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
spec :: Spec
spec = do

  -- entry variant preds
  describe "satisfies tests" $ do
    it "satisfies isDef" $
      example $ do
        (flip satisfies tDef <$> input Nothing Nothing [Just isDef]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isQuote" $
      example $ do
        (flip satisfies tQuote <$> input Nothing Nothing [Just isQuote]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isDialogue" $
      example $ do
        (flip satisfies tDialogue' <$> input Nothing Nothing [Just isDialogue]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isPhrase" $
      example $ do
        (flip satisfies tPhrase <$> input Nothing Nothing [Just isPhrase]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isDef or isQuote" $
      example $ do
        (flip satisfies tDef <$> input Nothing Nothing [Just isDef, Just isQuote]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isQuote or isDialogue or isPhrase" $
      example $ do
        (flip satisfies tQuote <$>
         input Nothing Nothing [Just isQuote, Just isDialogue, Just isPhrase]) >>=
          (`shouldBe` True)

  describe "satisfies tests" $ do
    it "satisfies isDialogue or isDef" $
      example $ do
        (flip satisfies tDialogue' <$> input Nothing Nothing [Just isDialogue, Just isDef]) >>=
          (`shouldBe` True)

  -- string search 
  describe "satisfies auth \"Woolf\"" $ do
    it "satisfies w auth search" $
      example $ do
        (flip satisfies tQuote <$> input (Just $ isInfixOf "Woolf") Nothing []) >>=
          (`shouldBe` True)

  -- string search 
  describe "satisfies auth \"Woolf\" but only return defs" $ do
    it "satisfies w auth search" $
      example $ do
        (flip satisfies tQuote <$> input (Just $ isInfixOf "Woolf") Nothing [Just isDef]) >>=
          (`shouldBe` False)

  describe "doesEntryMatchSearches \"Woolf\"" $ do
    it "satisfies w auth search" $
      example $ do
        (flip searchSatisfies tQuote <$> input (Just $ isInfixOf "Woolf") Nothing []) >>=
          (`shouldBe` True)

  -- string search 
  describe "satisfies auth \"Woolf\" but only return quotes" $ do
    it "satisfies w auth search" $
      example $ do
        (flip satisfies tQuote <$> input (Just $ isInfixOf "Woolf") Nothing [Just isQuote]) >>=
          (`shouldBe` True)
  -- string search 
  describe "doesReadSatisfy `Read`" $ do
    it "doesReadSatisfy" $
      example $ do
        (flip doesReadSatisfy tRead <$> input (Just $ isInfixOf "Woolf") Nothing []) >>=
          (`shouldBe` True)

  describe "all type filters applied" $ do
    it "all" $
      example $ do
        test
          Nothing
          Nothing
          [Just isPhrase, Just isQuote, Just isDef, Just isDialogue] >>=
          (`shouldBe` tDialogueFilter)

  describe "dialogue quote filtration" $ do
    it "tGetDialogueOrQuote" $
      example $ do
        test Nothing Nothing [Just isQuote, Just isDialogue] >>=
          (`shouldBe` tGetDialogueOrQuoteOut)

  describe "dialogue filtration" $ do
    it "tGetDialogue" $
      example $ do
        test Nothing Nothing [Just isDialogue] >>= (`shouldBe` tGetDialogueOut)
  describe "definition filtration" $ do
    it "tDefs" $
      example $ do test Nothing Nothing [Just isDef] >>= (`shouldBe` tDefs)
  describe "quote filtration" $ do
    it "tQuotes" $
      example $ do test Nothing Nothing [Just isQuote] >>= (`shouldBe` tQuotes)
  describe "phrases filtration" $ do
    it "tPhrases" $
      example $ do
        test Nothing Nothing [Just isPhrase] >>= (`shouldBe` tPhrases)
  describe "author match (for this test data should return all)" $ do
    it "tAuthor" $
      example $ do
        test (Just $ isInfixOf "Woolf") Nothing [] >>= (`shouldBe` tAuthor)

  -- combined type and search preds

  describe "-a Woolf -q" $ do
    it "tQuote" $
      example $ do
        test' (Just $ isInfixOf "Woolf") Nothing [Just isQuote] >>= (`shouldBe` authAndQuote)


tGetDialogueOrQuote :: IO [LogEntry]
tGetDialogueOrQuote = do
  i <- input (Just (isInfixOf "Woolf")) Nothing [Just isDialogue, Just isQuote]
  return $ filterWith i tDialogueFilter

tGetDialogue :: IO [LogEntry]
tGetDialogue = do
  i <- input (Just (isInfixOf "Woolf")) Nothing [Just isDialogue]
  i' <- input (Just (isInfixOf "Woolf")) Nothing [Just isDialogue]
  return $ filterWith i tDialogueFilter

tGetDialogueOrQuoteOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "Her simplicity fathomed what clever people falsified."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

tGetDialogueOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  ]

tDialogueFilter :: [LogEntry]
tDialogueFilter =
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
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Phr (Defined "some dashed barmy collocation" "aptly rummy sign"))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
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
      ( 0
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["ploverToplevel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

tDefs :: [LogEntry]
tDefs =
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
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["plover"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["ploverToplevel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"]))
  ]

tQuotes :: [LogEntry]
tQuotes =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "Her simplicity fathomed what clever people falsified."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

tPhrases =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Phr (Defined "some dashed barmy collocation" "aptly rummy sign"))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  ]

tAuthor :: [LogEntry]
tAuthor =
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
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Phr (Defined "some dashed barmy collocation" "aptly rummy sign"))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
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
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]

tDef =
  TabTsEntry
    (1, TimeStamp {hr = 10, min = 28, sec = 49}, Def (Defn Nothing ["plover"]))

tQuote =
  TabTsEntry
    ( 0
    , TimeStamp {hr = 10, min = 49, sec = 58}
    , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf"
        (Just 38))

tDialogue' =
  TabTsEntry
    ( 0
    , TimeStamp {hr = 8, min = 34, sec = 34}
    , Dialogue
        "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
tPhrase =
  TabTsEntry
    ( 1
    , TimeStamp {hr = 10, min = 17, sec = 40}
    , Phr (Defined "some dashed barmy collocation" "aptly rummy sign"))


sample :: [LogEntry]
sample =
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
      , TimeStamp {hr = 10, min = 17, sec = 40}
      , Phr (Defined "some dashed barmy collocation" "aptly rummy sign"))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 18, sec = 12}
      , Def (Defn (Just 38) ["sublime", "lintel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
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
      ( 0
      , TimeStamp {hr = 10, min = 28, sec = 49}
      , Def (Defn Nothing ["ploverToplevel"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 47, sec = 59}
      , Def (Defn Nothing ["cosmogony"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "I am merely coping with the collosal shame of having found out that I exist."
          "Keane Yahn-Krafft"
          (Just 38))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "I'm alive and it's not my fault, which means I must go on living the best I can,without bothering anybody, until I die."
          "In \"War and Peace\" by Leo Tolstoy"
          Nothing)
  ]

authAndQuote =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 24, sec = 2}
      , Quotation
          "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 55, sec = 6}
      , Read "To the Lighthouse" "Virginia Woolf")
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 25, sec = 27}
      , Quotation
          "Her simplicity fathomed what clever people falsified."
          "In \"To the Lighthouse\", by Virginia Woolf"
          Nothing)
    ]

tRead =
  TabTsEntry
    ( 0
    , TimeStamp {hr = 9, min = 55, sec = 6}
    , Read "To the Lighthouse" "Virginia Woolf")
