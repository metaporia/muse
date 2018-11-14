{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
 -----------------------------------------------------------------------------
-- |
-- Module      :  ParseSpec
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Tests parsers.
-----------------------------------------------------------------------------
module ParseSpec where

import Data.List (isInfixOf)
import Data.Time
import Helpers
import Parse
import Parse.Entry
import Prelude hiding (min)
import Search
import Test.Hspec
import Text.RawString.QQ
import qualified Text.Trifecta.Result as Tri
import Text.Trifecta
import Text.Show.Pretty (pPrint)

--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
spec :: Spec
spec = do
    describe "Timestamp parser" $ do
      it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]" $ do
        toMaybe (parse timestamp "00:11:22 λ. ") `shouldBe`
          (Just $ TimeStamp 00 11 22)
    -- testlog
    describe "Parse testlog" $ do
      it "parse entries testlog" $
        example $ do (toMaybe $ parse entries testlog) `shouldBe` (Just output)
    -- testlog with dump
    describe "Parse testlog including dumps" $ do
      it "parse logEntries testlog" $
        example $ do (toMaybe $ parse logEntries testlog) `shouldBe` (Just testLogWithDumpOutput')
    -- commentary
    describe "Parse commentary entry variant 1" $ do
      it "parse entries commentTs" $
        example $ do
          (toMaybe $ parse entries commentTs) `shouldBe` (Just commentTsOutput)
    -- commentary
    describe "Parse commentary entry variant 2" $ do
      it "parse entries commentTs'" $
        example $ do
          (toMaybe $ parse entries commentTs') `shouldBe`
            (Just commentTsOutput')
    -- pgNum
    describe "test pgNum: \"(s | e | p | f)<num>\"" $ do
      it "parse entries testPgNum" $
        example $ do
          (toMaybe $ parse entries testPgNum) `shouldBe`
            (Just testPgNumOutput) 
    -- skip lonely spaces
    describe "skip lines containing only spaces" $ do
      it "parse entries testLonelySpaces" $
        example $ do
          (toMaybe $ parse entries testLonelySpaces) `shouldBe`
            (Just testLonelySpacesOutput)

    describe "parse null entries (those w only timestamps)" $ do
      it "parse entries testLonelySpaces" $
        example $ do
          (toMaybe $ parse entries testNull) `shouldBe`
            (Just testNullOutput)

    describe "parse dump: \"...\n<multi-line-dump-body>\n...\"" $ do
      it "parse logEntries testDump" $
        example $ do
          (toMaybe $ parse logEntries testDump) `shouldBe`
            (Just testDumpOutput)

    describe "parse dump containing ellipsis" $ do
      it "parse logEntries tDump" $
        example $ do
          (toMaybe $ parse logEntries tDump) `shouldBe`
            (Just tDumpOut)

    describe "parse null timestamp" $ do
      it "parse logEntries tNull" $
        example $ do
          (toMaybe $ parse logEntries tNull) `shouldBe`
            (Just tNullOut)

    describe "keyword \"phrase\"" $ do
      it "parse logEntries tPhrase" $
        example $ do
          (toMaybe $ parse logEntries tPhrase) `shouldBe`
            (Just tPhraseOut)

    describe "keyword \"dialogue\"" $ do
      it "parse logEntries tDialogue" $
        example $ do
          (toMaybe $ parse logEntries tDialogue) `shouldBe`
            (Just tDialogueOut)

    describe "dialogue quote filtration" $ do
      it "tGetDialogue" $
        example $ do
          tGetDialogue >>= (`shouldBe` tGetDialogueOut)



tDialogue = [r|
13:36:33 λ. phrase sine qua non
    13:36:33 λ. phrase sine qua non
08:34:34 λ. dialogue

    (After ~1hr of unbridled loquacity, having mercifully dammed the torrent)
    MOM: Do you mind me telling all my favorite moments?

    (Without looking up from his guitar playing)
    DAD: No, just get it over with.

08:35:27 λ. read "Great Expectations" by Charles Dickens
 
|]

tDialogueOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n")
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 35, sec = 27}
      , Read "Great Expectations" "Charles Dickens")
  ]

tPhrase = [r|
13:36:33 λ. phrase sine qua non
10:55:26 λ. d raillery, coppice, disquisition, dissertation
13:36:33 λ. phr sine qua non
13:36:33 λ. phrase sine qua non : an essential condition
13:36:33 λ. phrase sine qua non: an essential condition

13:36:33 λ. d casement
|]

tPhraseOut =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 55, sec = 26}
      , Def
          (Defn Nothing ["raillery", "coppice", "disquisition", "dissertation"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"]))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non " "an essential condition"))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non" "an essential condition"))
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Def (Defn Nothing ["casement"]))
  ]

testLogWithDumpOutput' :: [LogEntry]
testLogWithDumpOutput' =
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
      (1, TimeStamp {hr = 10, min = 47, sec = 59}, Def (Defn Nothing ["cosmogony"]))
  , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 49, sec = 58}
      , Quotation
          "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
          "In \"To the Lighthouse\", by Virginia Woolf"
          (Just 38))
  ]


tGetDialogue :: IO [LogEntry]
tGetDialogue = do
  i <- input (Just (isInfixOf "Woolf")) Nothing [Just isDialogue, Just isQuote]
  return $ filterWith i tDialogueFilter

tGetDialogueOut =
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



testDump :: String
testDump = [r|
...
dump aeouoaeu
second line
...

    12:10:01 λ. d sylvan
...
dump body
multiple lines
... 
   
14:19:00 λ. read "Witches Abroad", by Terry Pratchett
 

|]

testDumpOutput :: [LogEntry]
testDumpOutput =
  [ Dump "\ndump aeouoaeu\nsecond line"
  , TabTsEntry
      (1, TimeStamp {hr = 12, min = 10, sec = 1}, Def (Defn Nothing ["sylvan"]))
  , Dump "\ndump body\nmultiple lines"
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 14, min = 19, sec = 0}
      , Read "Witches Abroad" "Terry Pratchett")
  ]


testNull :: String
testNull =
  [r|
    12:10:01 λ. 
|]

testNullOutput :: [(Int, TimeStamp, Entry)]
testNullOutput = [(1, TimeStamp {hr = 12, min = 10, sec = 1}, Null)]

testLonelySpaces :: String
testLonelySpaces = [r|
    12:10:01 λ. d sylvan
    

14:19:00 λ. read "Witches Abroad", by Terry Pratchett
 

 
|]

testLonelySpacesOutput :: [(Int, TimeStamp, Entry)]
testLonelySpacesOutput =
  [ (1, TimeStamp {hr = 12, min = 10, sec = 1}, Def (Defn Nothing ["sylvan"]))
  , ( 0
    , TimeStamp {hr = 14, min = 19, sec = 0}
    , Read "Witches Abroad" "Terry Pratchett")
  ]

-- `parse entries teslog`
output :: [(Int, TimeStamp, Entry)]
output =
  [ ( 0
    , TimeStamp {hr = 9, min = 55, sec = 6}
    , Read "To the Lighthouse" "Virginia Woolf")
  , ( 1
    , TimeStamp {hr = 9, min = 55, sec = 17}
    , Def
        (DefVersus
           "benignant"
           "kind; gracious; favorable;"
           "benign"
           "gentle, mild, or, medically, non-threatening"))
  , ( 1
    , TimeStamp {hr = 10, min = 11, sec = 45}
    , Def
        (DefVersus
           "malignant"
           "(adj.) disposed to inflict suffering or cause distress; inimical; bent on evil."
           "malign"
           "(adj.) having an evil disposition; spiteful; medically trheatening; (v.) to slander; to asperse; to show hatred toward."))
  , ( 1
    , TimeStamp {hr = 10, min = 17, sec = 40}
    , Def (Defn (Just 38) ["inimical", "traduce", "virulent"]))
  , ( 1
    , TimeStamp {hr = 10, min = 18, sec = 12}
    , Def (Defn (Just 38) ["sublime", "lintel"]))
  , ( 1
    , TimeStamp {hr = 10, min = 24, sec = 2}
    , Quotation
        "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
        "In \"To the Lighthouse\", by Virginia Woolf" Nothing)
  , ( 1
    , TimeStamp {hr = 10, min = 25, sec = 27}
    , Quotation
        "Her simplicity fathomed what clever people falsified."
        "In \"To the Lighthouse\", by Virginia Woolf" Nothing)
  , (1, TimeStamp {hr = 10, min = 28, sec = 49}, Def (Defn Nothing ["plover"]))
  , (1, TimeStamp {hr = 10, min = 47, sec = 59}, Def (Defn Nothing ["cosmogony"]))
  , ( 1
    , TimeStamp {hr = 10, min = 49, sec = 58}
    , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf" (Just 38))
  ]

commentTs :: String
commentTs =
  [r|20:30:00 λ. commentary
    
I found myself extremely aggravated by the claustrophobia-inducing parental
harassment Alan and Buddy's Father--with his anger--, and the Mother--with
her hypochondriacal whining. This repressive treatment--nay, parental
abuse--may have tapped long-suppressed issues of mine with authoritarian
hyper-management.

|]

commentTsOutput :: [(Int, TimeStamp, Entry)]
commentTsOutput =
  [ ( 0
    , TimeStamp {hr = 20, min = 30, sec = 0}
    , Commentary
        "I found myself extremely aggravated by the claustrophobia-inducing parental\nharassment Alan and Buddy's Father--with his anger--, and the Mother--with\nher hypochondriacal whining. This repressive treatment--nay, parental\nabuse--may have tapped long-suppressed issues of mine with authoritarian\nhyper-management.\n")
  ]

commentTs' :: String
commentTs' =
  [r|20:30:00 λ. synthesis

I found myself extremely aggravated by the claustrophobia-inducing parental
harassment Alan and Buddy's Father--with his anger--, and the Mother--with
her hypochondriacal whining. This repressive treatment--nay, parental
abuse--may have tapped long-suppressed issues of mine with authoritarian
hyper-management.

15:39:30 λ. d hello
|]

commentTsOutput' :: [(Int, TimeStamp, Entry)]
commentTsOutput' =
  [ ( 0
    , TimeStamp {hr = 20, min = 30, sec = 0}
    , Commentary
        "I found myself extremely aggravated by the claustrophobia-inducing parental\nharassment Alan and Buddy's Father--with his anger--, and the Mother--with\nher hypochondriacal whining. This repressive treatment--nay, parental\nabuse--may have tapped long-suppressed issues of mine with authoritarian\nhyper-management.\n")
  , (0, TimeStamp {hr = 15, min = 39, sec = 30}, Def (Defn Nothing ["hello"]))
  ]

-- test data
testStrDefn :: String
testStrDefn = [r| d word1, word2, hyphenated-word3 |]

testStrInline0 :: String
testStrInline0 = "d word1 : meaning1; meaning2; meaning3"

testStrInline1 :: String
testStrInline1 =
  [r|\
d word1 : meaning1; meaning2; meaning3;
  followup notes...

  NB: further commentary. all text to next TimeStamp should be lumped into the
  meaning
|]

testPgNum :: String
testPgNum = [r|
08:38:20 λ. p38 
08:38:20 λ. s 38 
08:38:20 λ. e38
08:38:20 λ.  f38 
08:38:20 λ. p  38
|]

testPgNumOutput :: [(Int, TimeStamp, Entry)]
testPgNumOutput = 
  [ ( 0 , TimeStamp { hr = 8 , min = 38 , sec = 20 } , PN (Page 38) )
  , ( 0
    , TimeStamp { hr = 8 , min = 38 , sec = 20 }
    , PN (PStart 38)
    )
  , ( 0 , TimeStamp { hr = 8 , min = 38 , sec = 20 } , PN (PEnd 38) )
  , ( 0
    , TimeStamp { hr = 8 , min = 38 , sec = 20 }
    , PN (PFinish 38)
    )
  , ( 0 , TimeStamp { hr = 8 , min = 38 , sec = 20 } , PN (Page 38) )
  ]

testLog :: String
testLog =
  [r|
08:23:30 λ. d quiescence, quiescent, quiesce
08:24:43 λ. d vouchsafed, another-word
08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal

08:38:20 λ. d elegy : meaning
08:45:37 λ. d tumbler

08:49:57 λ. d disport : meaning
08:56:30 λ. d larder
08:57:29 λ. d wainscot
09:12:16 λ. d fender
        09:14:12 λ. d bleat
        09:15:48 λ. d dissever
        09:24:04 λ. d rhapsody
09:15:48 λ. dvs deport : to transport, to carry away, to conduct (refl.)
            --- vs ---
            comport : to endure; carry together; to accord (with) |]

v0, v1, v0', v2, v2', v3, v3', v3'', v4, v5 :: String
v0 = "08:37:26 λ. d prorated, hello, mine, yours, hypochondriacal"

v0' = "prorated, hello, mine, yours, hypochondriacal"

v1 = "08:38:20 λ. d elegy"

v2 = "09:24:04 λ. d rhapsody : meaning1; meaning2;..."

v2' = "rhapsody : meaning1; meaning2;..."

v3' = "lèse majesté : meaning1; meaning2;..."

v3 =
  [r|
09:24:04 λ. quotation

            "There was no treachery too base for the world to commit. She knew
            that.  Happiness did not last. She knew that."

            Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]

v3'' =
  [r|quotation

        "There was no treachery too base for the world to commit. She knew
        that.  Happiness did not last. She knew that."

        Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]

v4 =
  [r|
dvs headword1 : meaning; aeousrcaheosruhoasuerh
    archoaeusrchaoeush roacheu rahue sarhue achue.
    --- vs ---
    headword2 : meaning; aeosrchu archeoau sraheou.

|]

v5 =
  [r| deport : to transport, to carry away, to conduct (refl.)
        --- vs ---
        comport : to endure; carry together; to accord (with)
|]

testlog :: String
testlog =
  [r|
09:55:06 λ. read "To the Lighthouse", by Virginia Woolf
    09:55:17 λ. dvs benignant : kind; gracious; favorable;
                    --- vs ---
                    benign : gentle, mild, or, medically, non-threatening
    10:11:45 λ. dvs malignant : (adj.) disposed to inflict suffering or cause
                distress; inimical; bent on evil.
                    --- vs ---
                    malign : (adj.) having an evil disposition; spiteful; 
                    medically trheatening; (v.) to slander; to asperse; to show
                    hatred toward.
    10:17:40 λ. d 38 inimical, traduce, virulent
    10:18:12 λ. d38 sublime, lintel
    10:24:02 λ. quotation
        
                "There was no treachery too base for the world to commit. She
                knew this. No happiness lasted."

                In "To the Lighthouse", by Virginia Woolf
    10:25:27 λ. q

                "Her simplicity fathomed what clever people falsified."
        
                In "To the Lighthouse", by Virginia Woolf
    10:28:49 λ. d plover
    10:47:59 λ. d cosmogony
    10:49:58 λ. q38
                
                "But nevertheless, the fact remained, that is was nearly
                impossbile to dislike anyone if one looked at them."

                In "To the Lighthouse", by Virginia Woolf

|]

tNull = [r|
 

21:32:05 λ.  

22:31:38 λ. quotation

        "I am merely coping with the collosal shame of having found out that I
        exist."

        Keane Yahn-Krafft
|]

tNullOut =
  [ TabTsEntry (0, TimeStamp {hr = 21, min = 32, sec = 5}, Null)
  , TabTsEntry
      ( 0
      , TimeStamp {hr = 22, min = 31, sec = 38}
      , Quotation
          "I am merely coping with the collosal shame of having found out that I exist."
          "Keane Yahn-Krafft"
          Nothing)
  ]

tDump = [r|
...
coffee
shit
fuck with ssh-keygen (error: "sign_and_send_pubkey: signing failed ...")
solution: `ssh-add` (the local agent simply needed a heads-up!).
...
|]

tDumpOut =
  [ Dump
      "\ncoffee\nshit\nfuck with ssh-keygen (error: \"sign_and_send_pubkey: signing failed ...\")\nsolution: `ssh-add` (the local agent simply needed a heads-up!)."
  ]

broken = [r|
...
coffee
shit
fuck with ssh-keygen (error: "sign_and_send_pubkey: signing failed ...")
solution: `ssh-add` (the local agent simply needed a heads-up!).
...
09:33:52 λ. muse-pre(the above) 
09:36:48 λ. read "Anna Kerenina", by Leo Tolstoy
    09:45:21 λ. s312
    09:53:29 λ. d316 winnowing
    09:56:56 λ. e316: dict conf
        10:04:53 λ. done: dict conf (see ~/dot/dictrc)
        
    10:05:13 λ. s316
    10:09:54 λ. d318 tarantas (sic). its really 'tarantass'
    10:27:58 λ. d329 interlocutor
    10:28:23 λ. d locution
    10:28:43 λ. e329
10:28:50 λ. shower; /p liam r. impends
11:32:11 λ. read "Anna Kerenina", by Leo Tolstoy
    11:32:27 λ. s329
    11:33:59 λ. d329 remunerative
    11:42:27 λ. d332 aperient
    11:43:12 λ. d332 catechism
    11:44:13 λ. d332 fractious
    11:50:28 λ. d334 commission
    11:54:41 λ. d334 splenetic
    12:01:27 λ. d339 turbid
    12:02:11 λ. d339 denuded
    12:05:03 λ. d340 unction
    12:06:56 λ. e341
12:07:01 λ. food
    12:07:55 λ. undone
12:08:10 λ. walk dogs
    12:35:21 λ. done
...
dad asked about college apps. Shame overwhelmed my affect. We talked. He said
he would not kill me, yet. What injustice. Mother needlessly involves herself.
She, as I in this aspect, has never intimated anything beyond herself. Perhaps
this is a human failing.

In any case, I beat the navy-blue garage door well-dented. My knuckles have
swollen accordingly. 

Hopefully Liam R. has shrooms.

drink gin 'n tonic (2 < shots)
...
14:12:35 λ. muse-interim
    14:17:13 λ. done
14:19:03 λ. read "Anna Kerenina", by Leo Tolstoy
    14:19:33 λ. s341
    14:24:16 λ. q343 
    
            "But neither of them dared speak of it, and not having
            expressed the one thing that occupied their thoughts, whatever they said
            rang false."

    14:26:55 λ. q344 
    
            "I am working, I want to do something, and I had forgotten it
            will all end in Death."

    14:30:29 λ. d345 cavilled
    14:32:01 λ. d carp
    14:32:34 λ. d345 dissimulation: the act of dissembling
    14:53:12 λ. d350 injunction
    14:57:43 λ. d350 brougham
    15:03:25 λ. d353 rejoined
    15:04:19 λ. q354 
    
            "One can see that he has been educated only to have the
            right to despise education, as they despise everything except animal
            pleasures." 

            Note: Is education not an animal pleasure, by our own bestiality and
            its inveterate tendency to promote education? Is not consciousness a form
            of self-referential edification, a perpetual apprehension of the latest
            'self'?

    15:19:20 λ. d361 jurisconsult
    15:26:30 λ. d363 furtive
    15:37:57 λ. d368 "piece de resistance"
    15:38:12 λ. e368
15:38:16 λ. walk dogs. /p liam r. impends +20 (arrival presumed @1600)

|]

p = parse logEntries
pp = pPrint . parse logEntries
