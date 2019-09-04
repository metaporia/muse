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

import           Control.Applicative
import           Data.Maybe                     ( fromJust )
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Time
import           Helpers
import           Parse
import           Parse.Entry
import           Prelude                 hiding ( min )
import           Store.Sqlite
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import           Text.Trifecta
import qualified Text.Trifecta.Result          as Tri

tparse :: String -> Result [LogEntry]
tparse = parse logEntries

test = hspec spec

--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
spec :: Spec
spec = do
  describe "Timestamp parser" $ do
    it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]" $ do
      toMaybe (parse timestamp "00:11:22 λ. ")
        `shouldBe` (Just $ TimeStamp 00 11 22)
    -- testlog
  describe "Parse testlog" $ do
    it "parse logEntries testlog" $ example $ do
      (toMaybe $ parse logEntries testlog) `shouldBe` (Just output)
    -- testlog with dump
  describe "Parse testlog including dumps" $ do
    it "parse logEntries testlog" $ example $ do
      (toMaybe $ parse logEntries testlog)
        `shouldBe` (Just testLogWithDumpOutput')
    -- commentary
  describe "Parse commentary entry variant 1" $ do
    it "parse logEntries commentTs" $ example $ do
      (toMaybe $ parse logEntries commentTs) `shouldBe` (Just commentTsOutput)
    -- commentary
  describe "Parse commentary entry variant 2" $ do
    it "parse logEntries commentTs'" $ example $ do
      (toMaybe $ parse logEntries commentTs') `shouldBe` (Just commentTsOutput')
    -- pgNum
  describe "test pgNum: \"(s | e | p | f)<num>\"" $ do
    it "parse logEntries testPgNum" $ example $ do
      (toMaybe $ parse logEntries testPgNum) `shouldBe` (Just testPgNumOutput)
    -- skip lonely spaces
  describe "skip lines containing only spaces" $ do
    it "parse logEntries testLonelySpaces" $ example $ do
      (toMaybe $ parse logEntries testLonelySpaces)
        `shouldBe` (Just testLonelySpacesOutput)
  describe "parse null logEntries (those w only timestamps)" $ do
    it "parse logEntries testNull" $ example $ do
      (toMaybe $ parse logEntries testNull) `shouldBe` (Just testNullOutput)
  describe "parse dump: \"...\n<multi-line-dump-body>\n...\"" $ do
    it "parse logEntries testDump" $ example $ do
      (toMaybe $ parse logEntries testDump) `shouldBe` (Just testDumpOutput)
  describe "parse dump containing ellipsis" $ do
    it "parse logEntries tDump" $ example $ do
      (toMaybe $ parse logEntries tDump) `shouldBe` (Just tDumpOut)
  describe "parse null timestamp" $ do
    it "parse logEntries tNull" $ example $ do
      (toMaybe $ parse logEntries tNull) `shouldBe` (Just tNullOut)
  describe "keyword \"phrase\"" $ do
    it "parse logEntries tPhrase" $ example $ do
      (toMaybe $ parse logEntries tPhrase) `shouldBe` (Just tPhraseOut)
  describe "keyword \"dialogue\"" $ do
    it "parse logEntries tDialogue" $ example $ do
      (toMaybe $ parse logEntries tDialogue) `shouldBe` (Just tDialogueOut)
  describe "unattributed quotations don't consume proceeding indentation" $ do
    it "parse logEntries autoAttr" $ example $ do
      (toMaybe $ parse logEntries autoAttr) `shouldBe` (Just autoAttrOut)
  describe "fail on entry without valid prefix" $ do
    it "parse logEntry tNoValidPrefix" $ example $ do
      (toMaybe $ parse logEntries tNoValidPrefix) `shouldBe` Nothing
  describe "square-bracketed, comma-separated tags" $ do
    -- "assert that tagChar may only be one of [a-zA-Z0-9-]" 
    it
        "* tagChar: alpha-numeric characters, spaces, underscores, and hyphens are valid `tagChar`s"
      $ do
          (toMaybe $ parse (many tagChar) "_- ") `shouldBe` Just "_- "
          (toMaybe $ parse (many tagChar) "abcdefghijklmnopqrstuvwxyz")
            `shouldBe` Just "abcdefghijklmnopqrstuvwxyz"
    it "* tag sequence may be empty, e.g., \"[]\" should parse successfully"
       ((toMaybe $ parse tags "[]") `shouldBe` Just [])
    it "* tag sequence may _not_ contain empty tags, as in \"[tag1,,]\""
       ((toMaybe $ parse tags "[tag1,,]") `shouldBe` Nothing)
    it
      "* example of well-formed tag list w whitespace trimming: \"[tag1,some_tag, my-special-tag, this is space separated ]\""
      (          (toMaybe $ parse
                   tags
                   "[tag1,some_tag, my-special-tag, this is space separated ]"
                 )
      `shouldBe` Just
                   [ "tag1"
                   , "some_tag"
                   , "my-special-tag"
                   , "this is space separated"
                   ]
      )


tNoValidPrefix = [r|
13:36:33 λ. not a prefix!
|]

tDialogue =
  [r|
13:36:33 λ. phrase sine qua non
    13:36:33 λ. phrase sine qua non
08:34:34 λ. dialogue

    (After ~1hr of unbridled loquacity, having mercifully dammed the torrent)
    MOM: Do you mind me telling all my favorite moments?

    (Without looking up from his guitar playing)
    DAD: No, just get it over with.

08:35:27 λ. read "Great Expectations" by Charles Dickens
 
|]

tDialogueOut
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 34, sec = 34}
      , Dialogue
        "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with.\n"
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 8, min = 35, sec = 27}
      , Read "Great Expectations" "Charles Dickens"
      )
    ]

tPhrase =
  [r|
13:36:33 λ. phrase sine qua non
10:55:26 λ. d raillery, coppice, disquisition, dissertation
13:36:33 λ. phr sine qua non
13:36:33 λ. phrase sine qua non : an essential condition
13:36:33 λ. phrase sine qua non: an essential condition

13:36:33 λ. d casement
|]

tPhraseOut
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 10, min = 55, sec = 26}
      , Def
        (Defn Nothing ["raillery", "coppice", "disquisition", "dissertation"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Plural ["sine qua non"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non " "an essential condition")
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Phr (Defined "sine qua non" "an essential condition")
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 33}
      , Def (Defn Nothing ["casement"])
      )
    ]

testLogWithDumpOutput' :: [LogEntry]
testLogWithDumpOutput'
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

testDump :: String
testDump =
  [r|
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
    , Read "Witches Abroad" "Terry Pratchett"
    )
  ]

testNull :: String
testNull =
  [r|
    12:10:01 λ. 
|]

testNullOutput :: [LogEntry]
testNullOutput = [TabTsEntry (0, TimeStamp {hr = 12, min = 10, sec = 1}, Null)]

testLonelySpaces :: String
testLonelySpaces =
  [r|
    

14:19:00 λ. read "Witches Abroad", by Terry Pratchett
 

    12:10:01 λ. d sylvan
 
|]

testLonelySpacesOutput :: [LogEntry]
testLonelySpacesOutput =
  [ TabTsEntry
    ( 0
    , TimeStamp {hr = 14, min = 19, sec = 0}
    , Read "Witches Abroad" "Terry Pratchett"
    )
  , TabTsEntry
    (1, TimeStamp {hr = 12, min = 10, sec = 1}, Def (Defn Nothing ["sylvan"]))
  ]

-- `parse entries teslog`
output :: [LogEntry]
output
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

commentTs :: String
commentTs =
  [r|20:30:00 λ. commentary
    
I found myself extremely aggravated by the claustrophobia-inducing parental
harassment Alan and Buddy's Father--with his anger--, and the Mother--with
her hypochondriacal whining. This repressive treatment--nay, parental
abuse--may have tapped long-suppressed issues of mine with authoritarian
hyper-management.

|]

commentTsOutput :: [LogEntry]
commentTsOutput =
  [ TabTsEntry
      ( 0
      , TimeStamp {hr = 20, min = 30, sec = 0}
      , Commentary
        "I found myself extremely aggravated by the claustrophobia-inducing parental\nharassment Alan and Buddy's Father--with his anger--, and the Mother--with\nher hypochondriacal whining. This repressive treatment--nay, parental\nabuse--may have tapped long-suppressed issues of mine with authoritarian\nhyper-management.\n"
      )
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

commentTsOutput' :: [LogEntry]
commentTsOutput'
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 20, min = 30, sec = 0}
      , Commentary
        "I found myself extremely aggravated by the claustrophobia-inducing parental\nharassment Alan and Buddy's Father--with his anger--, and the Mother--with\nher hypochondriacal whining. This repressive treatment--nay, parental\nabuse--may have tapped long-suppressed issues of mine with authoritarian\nhyper-management.\n"
      )
    , TabTsEntry
      (0, TimeStamp {hr = 15, min = 39, sec = 30}, Def (Defn Nothing ["hello"]))
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
testPgNum =
  [r|
08:38:20 λ. p38 
08:38:20 λ. s 38 
08:38:20 λ. e38
08:38:20 λ.  f38 
08:38:20 λ. p  38
|]

testPgNumOutput :: [LogEntry]
testPgNumOutput =
  [ TabTsEntry (0, TimeStamp {hr = 8, min = 38, sec = 20}, PN (Page 38))
  , TabTsEntry (0, TimeStamp {hr = 8, min = 38, sec = 20}, PN (PStart 38))
  , TabTsEntry (0, TimeStamp {hr = 8, min = 38, sec = 20}, PN (PEnd 38))
  , TabTsEntry (0, TimeStamp {hr = 8, min = 38, sec = 20}, PN (PFinish 38))
  , TabTsEntry (0, TimeStamp {hr = 8, min = 38, sec = 20}, PN (Page 38))
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

tNull =
  [r|
 

21:32:05 λ.  

22:31:38 λ. quotation

        "I am merely coping with the collosal shame of having found out that I
        exist."

        Keane Yahn-Krafft
|]

tNullOut
  = [ TabTsEntry (0, TimeStamp {hr = 21, min = 32, sec = 5}, Null)
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 22, min = 31, sec = 38}
      , Quotation
        "I am merely coping with the collosal shame of having found out that I exist."
        "Keane Yahn-Krafft"
        Nothing
      )
    ]

tDump =
  [r|
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

broken' =
  [r|
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

autoAttr = [r|

12:32:18 λ. d offering, sacrifice, expiate, propitiate, gift
13:32:17 λ. d expectorate, exsputory, exspuition
13:36:14 λ. d exorate, exoration, entreaty

13:38:51 λ. read "Pride and Prejudice" by Jane Austen
    13:39:05 λ. d ductility, stricture, negative, archness, arch, celerity
    13:41:46 λ. q

        "Happiness in marriage is entirely a matter of chance."

        In "Pride and Prejudice" by Jane Austen

    13:42:08 λ. q

        "Mary had neither genius nor taste; and though vanity had given her
        application, it had likewise given her a pedantic air and a conceited
        manner, which would have injured a higher degree of excellence than she
        had reached."
    13:43:38 λ. q
        "Your humility must disarm reproof."

    13:43:55 λ. q

        "To yield without conviction is no compliment to the understanding of
        either [party]."

|]

autoAttrOut
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 12, min = 32, sec = 18}
      , Def
        (Defn Nothing ["offering", "sacrifice", "expiate", "propitiate", "gift"]
        )
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 32, sec = 17}
      , Def (Defn Nothing ["expectorate", "exsputory", "exspuition"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 36, sec = 14}
      , Def (Defn Nothing ["exorate", "exoration", "entreaty"])
      )
    , TabTsEntry
      ( 0
      , TimeStamp {hr = 13, min = 38, sec = 51}
      , Read "Pride and Prejudice" "Jane Austen"
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 39, sec = 5}
      , Def
        (Defn
          Nothing
          ["ductility", "stricture", "negative", "archness", "arch", "celerity"]
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 41, sec = 46}
      , Quotation "Happiness in marriage is entirely a matter of chance."
                  "In \"Pride and Prejudice\" by Jane Austen"
                  Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 42, sec = 8}
      , Quotation
        "Mary had neither genius nor taste; and though vanity had given her application, it had likewise given her a pedantic air and a conceited manner, which would have injured a higher degree of excellence than she had reached."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 43, sec = 38}
      , Quotation "Your humility must disarm reproof." "" Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 13, min = 43, sec = 55}
      , Quotation
        "To yield without conviction is no compliment to the understanding of either [party]."
        ""
        Nothing
      )
    ]

p = parse logEntries

pp = pPrint . parse logEntries

br = [r|"Mary had neither genius nor taste; and though vanity had given her
application, it had \"likewise\" given her a pedantic air and a conceited
manner, which would have injured a higher degree of excellence than she
had reached."|]

broke = [r|

        "Mary had neither genius nor taste; and though vanity had given her
        application, it had likewise given her a pedantic air and a conceited
        manner, which would have injured a higher degree of excellence than she
        had reached."

    13:43:38 λ. q

        "Your humility must disarm reproof."
|]

 
af = [r|
00:12:45 λ. d hermetic
00:20:34 λ. d bona fide
01:55:20 λ. d fenestrate
02:10:52 λ. d coventry
02:56:42 λ. d reliquary

17:46:19 λ. read "Wuthering Heights" by Emily Brontë
    17:46:22 λ. q

        "any guest that is safe from repeating his visit can generally be
        made welcome."

    17:47:16 λ. d stark, bevy
    17:47:50 λ. d respire, inspire

17:55:37 λ. begin to read "Warlock" by Oakley Hall
    17:55:48 λ. d prefatory, skirl
    18:10:04 λ. d pertinacious, pert, apert, impertinent, pertinent
    18:11:25 λ. d obdurate, obstinate

18:38:51 λ. d wherefore
18:43:51 λ. d refraining, nephalism

20:10:07 λ. read "Warlock" by Oakley Hall
    20:12:29 λ. d frock
    20:12:38 λ. d pasear: (spa.) to (take a) walk
    20:16:04 λ. d concho: a silver, ornamented, convex ellipsoid; often
                          found on the belts of the garish
    20:23:10 λ. d cheroot, blowhard
    20:27:15 λ. d disposition, dispose, dis-
    20:31:39 λ. d phr "congealing silence"
    20:31:45 λ. d louvre
    20:44:36 λ. d snub (2)
    20:51:25 λ. d suspirant: of sighs
    20:51:30 λ. d mezzotint : metal engraving
    20:54:18 λ. d daguerreotype: early photograph
    20:54:28 λ. d lithograph: copy, ink-transfer of existing image; a print
    20:57:43 λ. d olla, stricture
23:53:05 λ. d gnostic
|]

broken = [r|
09:44:14 λ. read "Pride And Prejudice" by Jane Austen
    09:44:45 λ. d repine, abseil
    09:45:07 λ. d repute, dispute, compute, impute, putative
    09:49:36 λ. d disoblige
    09:51:08 λ. d perforce
    09:51:15 λ. d manifold
    09:52:32 λ. q

        "Is not general incivility the very essence of love?"

    09:54:11 λ. q

        "There are few people whom I really love and fewer still of whom I
        think well."

    09:54:32 λ. q

        "We must not be so ready to fancy ourselves intentionally injured."

    09:54:53 λ. q
    
        "The stupidity with which he was favored by nature, must guard his
        courtship from any charm that could make a woman wish for its
        continuance."

    10:11:00 λ. phr "as well by...as..."
    10:11:22 λ. phr "despair of"
    10:11:33 λ. phr "savors of"
    10:13:40 λ. d insupportable
    10:14:18 λ. d paling
    10:27:13 λ. d trepidity, trepid
    14:10:42 λ. d fortnight
    14:20:14 λ. d politic, impolitic
    14:21:21 λ. dvs celerity : rapidity of motion; swiftness
                    --- vs ---
                    alacrity : cheerful readiness; sprightliness; promptness
                    
    15:03:36 λ. d rencontre, rencounter
    15:06:09 λ. d immure, durance, inure
    15:10:55 λ. d tractable
    15:11:31 λ. d officious
    15:16:31 λ. d intimate
    15:26:12 λ. d verdure
    15:49:25 λ. d obeisance, obedience
    15:53:15 λ. d derogate, derogation
    15:57:40 λ. phr "hopeless of remedy"
    15:57:56 λ. d upbraid
    16:17:37 λ. q

        "One may be continually abusive to a man without saying any thing just;
        but one cannot be always laughing at a man without now and then
        stumbling upon something witty."

    16:18:34 λ. d ecstasy
    16:31:17 λ. dvs reproof : expression of blame or censure; refutation,
                    --- vs ---
                    reproach: to blame, censure severely or contemptuously.
                    ("reproach" seems the stronger term; reproof is orthogonal
                    to civility, whereas reproach connotes a conveyance of
                    contempt, disdain, or disgrace--not necessarily, but such
                    are present in its definition.)

    16:38:23 λ. dvs confute : to quell to silence, stillness, or surrender; to allay; 
                    literally, to stop from boiling (both derive from "fuse" as "to melt")
                    --- vs ---
                    refute : to repel; to disprove by argument or evidence

    16:52:03 λ. d rather, injunction
    17:02:35 λ. phr "bent their steps thither"
    17:06:35 λ. d ovate, obovate
    17:15:53 λ. d coppice
    17:22:38 λ. d occasion
    17:59:26 λ. d diffidence
    18:43:22 λ. d paddock
    20:03:19 λ. d connubial
    20:31:28 λ. d abominate
    20:39:00 λ. d canvass
    20:52:09 λ. q

        "We all love to instruct, though we can teach only what is not worth
        knowing."

    21:57:56 λ. q

        "But in such cases as these, a good memory is unpardonable. This is the
        last time I shall ever remember it myself."


    22:08:02 λ. q
    
        "How unlucky that you should have a reasonable answer to give, and that
        I should be so reasonable as to admit it!"

    22:08:52 λ. phr "their characters suffered no revolution"
|]

brokenOut
  = [ TabTsEntry
      ( 0
      , TimeStamp {hr = 9, min = 44, sec = 14}
      , Read "Pride And Prejudice" "Jane Austen"
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 44, sec = 45}
      , Def (Defn Nothing ["repine", "abseil"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 45, sec = 7}
      , Def
        (Defn Nothing ["repute", "dispute", "compute", "impute", "putative"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 49, sec = 36}
      , Def (Defn Nothing ["disoblige"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 51, sec = 8}
      , Def (Defn Nothing ["perforce"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 51, sec = 15}
      , Def (Defn Nothing ["manifold"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 52, sec = 32}
      , Quotation "Is not general incivility the very essence of love?"
                  ""
                  Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 54, sec = 11}
      , Quotation
        "There are few people whom I really love and fewer still of whom I think well."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 54, sec = 32}
      , Quotation
        "We must not be so ready to fancy ourselves intentionally injured."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 9, min = 54, sec = 53}
      , Quotation
        "The stupidity with which he was favored by nature, must guard his courtship from any charm that could make a woman wish for its continuance."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 0}
      , Phr (Plural ["\"as well by...as...\""])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 22}
      , Phr (Plural ["\"despair of\""])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 11, sec = 33}
      , Phr (Plural ["\"savors of\""])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 13, sec = 40}
      , Def (Defn Nothing ["insupportable"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 14, sec = 18}
      , Def (Defn Nothing ["paling"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 10, min = 27, sec = 13}
      , Def (Defn Nothing ["trepidity", "trepid"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 14, min = 10, sec = 42}
      , Def (Defn Nothing ["fortnight"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 14, min = 20, sec = 14}
      , Def (Defn Nothing ["politic", "impolitic"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 14, min = 21, sec = 21}
      , Def
        (DefVersus "celerity"
                   "rapidity of motion; swiftness"
                   "alacrity"
                   "cheerful readiness; sprightliness; promptness"
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 3, sec = 36}
      , Def (Defn Nothing ["rencontre", "rencounter"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 6, sec = 9}
      , Def (Defn Nothing ["immure", "durance", "inure"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 10, sec = 55}
      , Def (Defn Nothing ["tractable"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 11, sec = 31}
      , Def (Defn Nothing ["officious"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 16, sec = 31}
      , Def (Defn Nothing ["intimate"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 26, sec = 12}
      , Def (Defn Nothing ["verdure"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 49, sec = 25}
      , Def (Defn Nothing ["obeisance", "obedience"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 53, sec = 15}
      , Def (Defn Nothing ["derogate", "derogation"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 57, sec = 40}
      , Phr (Plural ["\"hopeless of remedy\""])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 15, min = 57, sec = 56}
      , Def (Defn Nothing ["upbraid"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 16, min = 17, sec = 37}
      , Quotation
        "One may be continually abusive to a man without saying any thing just; but one cannot be always laughing at a man without now and then stumbling upon something witty."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 16, min = 18, sec = 34}
      , Def (Defn Nothing ["ecstasy"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 16, min = 31, sec = 17}
      , Def
        (DefVersus
          "reproof"
          "expression of blame or censure; refutation,"
          "reproach"
          "to blame, censure severely or contemptuously. (\"reproach\" seems the stronger term; reproof is orthogonal to civility, whereas reproach connotes a conveyance of contempt, disdain, or disgrace--not necessarily, but such are present in its definition.)"
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 16, min = 38, sec = 23}
      , Def
        (DefVersus
          "confute"
          "to quell to silence, stillness, or surrender; to allay; literally, to stop from boiling (both derive from \"fuse\" as \"to melt\")"
          "refute"
          "to repel; to disprove by argument or evidence"
        )
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 16, min = 52, sec = 3}
      , Def (Defn Nothing ["rather", "injunction"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 2, sec = 35}
      , Phr (Plural ["\"bent their steps thither\""])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 6, sec = 35}
      , Def (Defn Nothing ["ovate", "obovate"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 15, sec = 53}
      , Def (Defn Nothing ["coppice"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 22, sec = 38}
      , Def (Defn Nothing ["occasion"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 17, min = 59, sec = 26}
      , Def (Defn Nothing ["diffidence"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 18, min = 43, sec = 22}
      , Def (Defn Nothing ["paddock"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 20, min = 3, sec = 19}
      , Def (Defn Nothing ["connubial"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 20, min = 31, sec = 28}
      , Def (Defn Nothing ["abominate"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 20, min = 39, sec = 0}
      , Def (Defn Nothing ["canvass"])
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 20, min = 52, sec = 9}
      , Quotation
        "We all love to instruct, though we can teach only what is not worth knowing."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 21, min = 57, sec = 56}
      , Quotation
        "But in such cases as these, a good memory is unpardonable. This is the last time I shall ever remember it myself."
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 22, min = 8, sec = 2}
      , Quotation
        "How unlucky that you should have a reasonable answer to give, and that I should be so reasonable as to admit it!"
        ""
        Nothing
      )
    , TabTsEntry
      ( 1
      , TimeStamp {hr = 22, min = 8, sec = 52}
      , Phr (Plural ["\"their characters suffered no revolution\""])
      )
    ]
