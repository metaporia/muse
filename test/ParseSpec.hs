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
import           Data.Time
import           Helpers
import           Parse
import           Parse.TestData
import           Parse.Types
import           Parse.Entry             hiding ( logEntries )
import           Prelude                 hiding ( min )
import           Store.Sqlite
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )
import           Text.Trifecta
import qualified System.IO                     as SysIO
import qualified Text.Trifecta.Result          as Tri

tparse :: String -> Result [LogEntry]
tparse = parse logEntries

test = hspec spec
logEntries = validatedLogEntries

p = parse logEntries

pp = pPrint . parse logEntries
--import Text.Trifecta.Result (Result(..))
--import Test.QuickCheck
spec :: Spec
spec = do
  describe "Timestamp parser"
    $          it "parse timestamp \"00:11:22 λ. \" is [00, 11, 22]"
    $          toMaybe (parse timestamp "00:11:22 λ. ")
    `shouldBe` (Just $ TimeStamp 00 11 22)
    -- testlog
  describe "Parse testlog"
    $          it "parse logEntries testlog"
    $          example
    $          toMaybe (parse logEntries testlog)
    `shouldBe` Just output
    -- testlog with dump
  describe "Parse testlog including dumps"
    $          it "parse logEntries testlog"
    $          example
    $          toMaybe (parse logEntries testlog)
    `shouldBe` Just testLogWithDumpOutput'
    -- commentary
  describe "Parse commentary entry variant 1"
    $          it "parse logEntries commentTs"
    $          example
    $          toMaybe (parse logEntries commentTs)
    `shouldBe` Just commentTsOutput
    -- commentary
  describe "Parse commentary entry variant 2"
    $          it "parse logEntries commentTs'"
    $          example
    $          toMaybe (parse logEntries commentTs')
    `shouldBe` Just commentTsOutput'
    -- pgNum
  describe "test pgNum: \"(s | e | p | f)<num>\""
    $          it "parse logEntries testPgNum"
    $          example
    $          toMaybe (parse logEntries testPgNum)
    `shouldBe` Just testPgNumOutput
    -- skip lonely spaces
  describe "skip lines containing only spaces"
    $          it "parse logEntries testLonelySpaces"
    $          example
    $          toMaybe (parse logEntries testLonelySpaces)
    `shouldBe` Just testLonelySpacesOutput
  describe "parse null logEntries (those w only timestamps)"
    $ it "parse logEntries testNull"
    $ example
    $ do
        let results = parse logEntries testNull
        SysIO.hPutStrLn SysIO.stderr $ ppShow results
        toMaybe results `shouldBe` Just testNullOutput
  describe "parse dump: \"...\n<multi-line-dump-body>\n...\""
    $          it "parse logEntries testDump"
    $          example
    $          toMaybe (parse logEntries testDump)
    `shouldBe` Just testDumpOutput
  describe "parse dump containing ellipsis"
    $          it "parse logEntries tDump"
    $          example
    $          toMaybe (parse logEntries tDump)
    `shouldBe` Just tDumpOut
  describe "parse null timestamp"
    $          it "parse logEntries tNull"
    $          example
    $          toMaybe (parse logEntries tNull)
    `shouldBe` Just tNullOut
  describe "keyword \"phrase\""
    $          it "parse logEntries tPhrase"
    $          example
    $          toMaybe (parse logEntries tPhrase)
    `shouldBe` Just tPhraseOut
  describe "keyword \"dialogue\""
    $          it "parse logEntries tDialogue"
    $          example
    $          toMaybe (parse logEntries tDialogue)
    `shouldBe` Just tDialogueOut
  describe "unattributed quotationsn't consume proceeding indentation"
    $          it "parse logEntries autoAttr"
    $          example
    $          toMaybe (parse logEntries autoAttr)
    `shouldBe` Just autoAttrOut
  describe "fail on entry without valid prefix"
    $          it "parse logEntry tNoValidPrefix"
    $          example
    $          toMaybe (parse (logEntries <* eof) tNoValidPrefix) -- FIXME example of poor error message
    `shouldBe` Nothing
  describe "square-bracketed, comma-separated tags" $ do
    -- "assert that tagChar may only be one of [a-zA-Z0-9-]"
    it
        "* tagChar: alpha-numeric characters, spaces, underscores, and hyphens are valid `tagChar`s"
      $ do
          toMaybe (parse (many tagChar) "_- ") `shouldBe` Just "_- "
          toMaybe (parse (many tagChar) "abcdefghijklmnopqrstuvwxyz")
            `shouldBe` Just "abcdefghijklmnopqrstuvwxyz"
    it "* tag sequence may be empty, e.g., \"[]\" should parse successfully"
       (toMaybe (parse tags "[]") `shouldBe` Just [])
    it "* tag sequence may _not_ contain empty tags, as in \"[tag1,,]\""
       (toMaybe (parse tags "[tag1,,]") `shouldBe` Nothing)
    it
      "* example of well-formed tag list w whitespace trimming: \"[tag1,some_tag, my-special-tag, this is space separated ]\""
      (          toMaybe
          (parse tags
                 "[tag1,some_tag, my-special-tag, this is space separated ]"
          )
      `shouldBe` Just
                   [ "tag1"
                   , "some_tag"
                   , "my-special-tag"
                   , "this is space separated"
                   ]
      )

