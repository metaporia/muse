{-# LANGUAGE OverloadedStrings #-}

module CLI.ParseSpec where

import           CLI.Parser.Custom              ( parseTags )
import           Test.Hspec
import           Test.Hspec.Megaparsec          ( shouldFailOn
                                                , shouldParse
                                                )
import           Text.Megaparsec                ( runParser )

test = hspec spec

pt = flip runParser ""

spec :: Spec
spec = do
  describe "CLI's tag list parser" $ do
    it "single tag" $ pt parseTags "phrase" `shouldParse` ["phrase"]
    it "double tag w special chars"
      $             pt parseTags "a_phrase,tags/my-tag"
      `shouldParse` ["a_phrase", "tags/my-tag"]
    -- FIXME duplicate functionality merge 'parseTags' and 'parseTags' (blocked
    -- on replacement of String with Text in SearchConfig).
    it "(should fail) double w space 0: \" phrase,french\""
      $              pt parseTags
      `shouldFailOn` " phrase,french"
    it "(should fail) double w space 1: \"phrase ,french\""
      $              pt parseTags
      `shouldFailOn` "phrase ,french"
    it "(should fail) double w space 2: \"phrase, french\""
      $              pt parseTags
      `shouldFailOn` "phrase, french"
    it "(should fail) double w space 3: \"phrase,french \""
      $              pt parseTags
      `shouldFailOn` "phrase,french "
