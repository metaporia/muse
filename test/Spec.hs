{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Parse
import Text.Trifecta.Result (Result(..))
import qualified Text.Trifecta.Result as Tri

toMaybe :: Tri.Result a -> Maybe a
toMaybe (Tri.Success a) = Just a
toMaybe (Tri.Failure a) = Nothing

defMultWords = undefined

main :: IO ()
main = hspec $ do
  -- parser
  describe "Timestamp parser" $ do
    it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]" $ do
      toMaybe (parse timestamp "00:11:22 λ. ") `shouldBe` (Just $ TimeStamp 00 11 22)

  -- definition query (i.) multi word list
  describe "Parse definiton query: multiple words" $ do
    it "headwords \"d word1, word2, hyphenated-word3\"" $ do
    --   \n\\[ Def (\"word1\", Nothing) \
    --   \n\ , Def (\"word2\", Nothing) \
    --   \n\ , Def (\"hyphenated-word3\", Nothing)\]" $ do
         headwords "d word1, word2, hyphenated-word3"
           `shouldBe` [ "word1"
                      , "word2"
                      , "hyphenated-word3"]
