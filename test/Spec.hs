{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Parse
import Prelude hiding (min)
import Test.Hspec
import Text.RawString.QQ
import qualified Text.Trifecta.Result as Tri

--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
main :: IO ()
main =
  hspec $ do
    describe "Timestamp parser" $ do
      it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]" $ do
        toMaybe (parse timestamp "00:11:22 λ. ") `shouldBe`
          (Just $ TimeStamp 00 11 22)
    -- testlog
    describe "Parse testlog" $ do
      it "parse entries testlog" $
        example $ do (toMaybe $ parse entries testlog) `shouldBe` (Just output)
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
           "(adj.) disposed to inflict suffering or cause\n                distress; inimical; bent on evil."
           "malign"
           "(adj.) having an evil disposition; spiteful; \n                    medically trheatening; (v.) to slander; to asperse; to show\n                    hatred toward."))
  , ( 1
    , TimeStamp {hr = 10, min = 17, sec = 40}
    , Def (Defn ["inimical", "traduce", "virulent"]))
  , ( 1
    , TimeStamp {hr = 10, min = 18, sec = 12}
    , Def (Defn ["sublime", "lintel"]))
  , ( 1
    , TimeStamp {hr = 10, min = 24, sec = 2}
    , Quotation
        "There was no treachery too base for the world to commit. She knew this. No happiness lasted."
        "In \"To the Lighthouse\", by Virginia Woolf")
  , ( 1
    , TimeStamp {hr = 10, min = 25, sec = 27}
    , Quotation
        "Her simplicity fathomed what clever people falsified."
        "In \"To the Lighthouse\", by Virginia Woolf")
  , (1, TimeStamp {hr = 10, min = 28, sec = 49}, Def (Defn ["plover"]))
  , (1, TimeStamp {hr = 10, min = 47, sec = 59}, Def (Defn ["cosmogony"]))
  , ( 1
    , TimeStamp {hr = 10, min = 49, sec = 58}
    , Quotation
        "But nevertheless, the fact remained, that is was nearly impossbile to dislike anyone if one looked at them."
        "In \"To the Lighthouse\", by Virginia Woolf")
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
  , (0, TimeStamp {hr = 15, min = 39, sec = 30}, Def (Defn ["hello"]))
  ]

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure _) = Nothing

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
    10:17:40 λ. d inimical, traduce, virulent
    10:18:12 λ. d sublime, lintel
    10:24:02 λ. quotation
        
                "There was no treachery too base for the world to commit. She
                knew this. No happiness lasted."

                In "To the Lighthouse", by Virginia Woolf
    10:25:27 λ. quotation

                "Her simplicity fathomed what clever people falsified."
        
                In "To the Lighthouse", by Virginia Woolf
    10:28:49 λ. d plover
    10:47:59 λ. d cosmogony
    10:49:58 λ. quotation
                
                "But nevertheless, the fact remained, that is was nearly
                impossbile to dislike anyone if one looked at them."

                In "To the Lighthouse", by Virginia Woolf

|]
