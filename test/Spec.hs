{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Hspec
--import Test.QuickCheck
import Text.RawString.QQ
import Parse
--import Text.Trifecta.Result (Result(..))
import qualified Text.Trifecta.Result as Tri

main :: IO ()
main = hspec $ do
  -- parser
  describe "Timestamp parser" $ do
    it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]" $ do
      toMaybe (parse timestamp "00:11:22 λ. ") `shouldBe` (Just $ TimeStamp 00 11 22)



toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure _) = Nothing


-- test data

testStrDefn :: String
testStrDefn = [r| d word1, word2, hyphenated-word3 |]

testStrInline0  :: String
testStrInline0 = "d word1 : meaning1; meaning2; meaning3"

testStrInline1 :: String
testStrInline1 = [r|\
d word1 : meaning1; meaning2; meaning3;
  followup notes...

  NB: further commentary. all text to next TimeStamp should be lumped into the
  meaning
|]

testLog :: String
testLog = [r|
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

v3 = [r|
09:24:04 λ. quotation

            "There was no treachery too base for the world to commit. She knew
            that.  Happiness did not last. She knew that."

            Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]

v3'' = [r|quotation

        "There was no treachery too base for the world to commit. She knew
        that.  Happiness did not last. She knew that."

        Mrs. Ramsey in "To the Lighthouse", by Virgina Woolf
|]

v4 = [r|
dvs headword1 : meaning; aeousrcaheosruhoasuerh
    archoaeusrchaoeush roacheu rahue sarhue achue.
    --- vs ---
    headword2 : meaning; aeosrchu archeoau sraheou.

|]

v5 = [r| deport : to transport, to carry away, to conduct (refl.)
        --- vs ---
        comport : to endure; carry together; to accord (with)
|]



