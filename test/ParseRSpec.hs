{-# LANGUAGE QuasiQuotes #-}

module ParseRSpec where

import           ParseR                  hiding ( pt
                                                , curr
                                                )
import           Parse.Types                    ( Entry(..)
                                                , DefQuery(..)
                                                )
import           Prelude                 hiding ( read )
import           Test.Hspec
import           Text.Megaparsec.Char           ( newline )
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )

test = hspec spec 
pt = flip parse ""

spec :: Spec
spec = do
  describe "read" $ do
    let eagleton = Read "The Ideology of the Aesthetic" "Terry Eagleton"
    it "read by"
      $ pt read "read \"The Ideology of the Aesthetic\" by Terry Eagleton\n"
      `shouldParse` eagleton
    it "begin by"
      $ pt read "begin \"The Ideology of the Aesthetic\" by Terry Eagleton\n"
      `shouldParse` eagleton
    it "finish by"
      $ pt read "finish \"The Ideology of the Aesthetic\" by Terry Eagleton\n"
      `shouldParse` eagleton
    it "begin to read by"
      $             pt
                      read
                      "begin to read \"The Ideology of the Aesthetic\" by Terry Eagleton\n"
      `shouldParse` eagleton
    it "finish reading by"
      $             pt
                      read
                      "finish reading \"The Ideology of the Aesthetic\" by Terry Eagleton\n"
      `shouldParse` eagleton
  describe "textBlock" $ do
    it "indented text block"
      $             pt indentedTextBlock indentedV0
      `shouldParse` ("this\nis\na\n\n  \n\nindented\ntext\nblock", 2)
    it "fenced text block" $ do
      pt fencedTextBlock fenceV0
        `shouldParse` "text block\n\nof \n\nmany \n\n\n\nlines"
      pt fencedTextBlock (fenceV0 ++ "  \n \n\n\n  \n")
        `shouldParse` "text block\n\nof \n\nmany \n\n\n\nlines"
      pt fencedTextBlock fenceV1
        `shouldParse` "text block\n\nof \n\nmany \n\n  \n\nlines"
    it "commentary" $ do
      pt commentary commentary0 `shouldParse` Commentary
        "Fenced\n\ntext area\nFenced\nFenced\nFenced\n"

      pt commentary commentary1 `shouldParse` Commentary
        "Fenced\n\ntext area\nFenced\nFenced\nFenced\n"

    it "dialogue"
      $             pt dialogue dialogue0
      `shouldParse` Dialogue
                      "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with."

  describe "emptyLine"
    $             it "skip newlines and lines with spaces"
    $             pt (many (try emptyLine)) emptyLineEx
    `shouldParse` [ "\n"
                  , "\n"
                  , "    \n"
                  , "\n"
                  , "\n"
                  , "        \n"
                  , "  \n"
                  , "\n"
                  , "\n"
                  , "  \n"
                  , "\n"
                  , "\n"
                  ]
  describe "quotation" $ do
    it "multiline, skip space, no attr, pg"
      $             pt quotation multiSkipNoAttrPg
      `shouldParse` Quotation "quote \n\nk\n\nbody \ncont." "" (Just 23)
    it "multiline, no skip, no attr"
      $             pt quotation multiNoSkipNoAttrNoPg
      `shouldParse` Quotation "quote \nk\nbody \ncont." "" Nothing
    it "multiline, skip, attr, no pg"
      $             pt quotation multiSkipAttrNoPg
      `shouldParse` Quotation "quote\n\nbody\ncont." "attr" Nothing
    it "quote content, multi, skip"
      $ pt quoteContent qcMultiSkipTrim
      `shouldParse` ( "this is the\nquote.\n\n\n\n   \n\n\n\n       \n\nSpanning multiple\nlines."
                    , 4
                    )
  describe "definition" $ do
    it "hw, pg, single" $ pt headwords hws0 `shouldParse` Defn (Just 53)
                                                               ["somatic"]
    it "hw, pg, multi" $ pt headwords hws1 `shouldParse` Defn
      (Just 53)
      ["somatic", "sensate", "endue"]
    it "hw, no pg, multi" $ pt headwords hws2 `shouldParse` Defn
      Nothing
      ["somatic", "sensate", "endue"]
    it "hw, pg no right-padding, single" $ pt headwords hws3 `shouldParse` Defn
      (Just 45)
      ["somatic", "sensate"]
    it "inline, multi heawords w punc; multi-line meaning w spaces"
      $ pt (inlineMeaning False) inline0
      `shouldParse` ( "some headword \"'\" allowed!"
                    , "multi-line\nmeaning \nas long\n  as the indentation\n\n\n\nis greater than or equal to that of the first line"
                    )
    it "indentedTextBlockUntil" $ do
      let
        p =
          pt
            (some $ indentedTextBlockUntil $ Just
              (defVersusSeparator <* newline)
            )
      p terminatedTextBlock
        `shouldParse` [ ( "Note: \"decency\" works better in place of \"decorum\" than the\n\nconverse. It additionally means sufficient, respectable,\nfairly good, of which meaning \"decorum\" is without.\nOne had better employ \"decorum\" to describe proper\n"
                        , 4
                        )
                      , ( "conformance to propriety, and \"decent\" (bar its extra\nmeaning) to the genuine article, whose outward show of\ndecorum and propriety is in no way of artifice but merely\nan unmpremeditated projection of inner material."
                        , 4
                        )
                      ]
      p threeTextBlocks
        `shouldParse` [ ("text block 1\nand more\n"       , 0)
                      , ("text block 2\n\nand some more\n", 0)
                      , ("text block 3\nla di da"         , 0)
                      ]

    it "defVersus w multi paragraph second meaning"
      $             pt definition defVersus0
      `shouldParse` Def
                      (DefVersus
                        "decorum"
                        "outward grace or suitableness of conduct, subscription \nto propriety\n"
                        "    decency"
                        "having the quality of being moderate,\nseemly, becoming; free from immodesty or obscenity;\nproper formality\n\nNote: \"decency\" works better in place of \"decorum\" than the\nconverse. It additionally means sufficient, respectable,\nfairly good, of which meaning \"decorum\" is without.\nOne had better employ \"decorum\" to describe proper\nconformance to propriety, and \"decent\" (bar its extra\nmeaning) to the genuine article, whose outward show of\ndecorum and propriety is in no way of artifice but merely\nan unmpremeditated projection of inner material.\n\ntl;dr; \"decency\" has a (positive) moral connotation"
                      )



emptyLineEx = [r|

    


        
  


  


|]

multiSkipNoAttrPg = [r|q 23

        "quote 
        k

        body 

        cont."

   

|]

multiNoSkipNoAttrNoPg = [r|q

        "quote 
        k
        body 
        cont."

   

|]

multiSkipAttrNoPg = [r|q
  "quote
  body

  cont."

  attr

|]

literal = [r|"hello

world"|]

qcMultiSkipTrim = [r|    "this is the
    quote.
    Spanning multiple

  

       
    

   
           

    lines."


|]

fenceV0 = [r|```
text block

of 

many 



lines```|]

fenceV1 = [r|```text block

of 

many 

  

lines```|]



indentedV0 = [r|  this
  is
  a

    

  indented
  text
  block

t
|]

dialogue0 = [r|dialogue

    (After ~1hr of unbridled loquacity, having mercifully dammed the torrent)
    MOM: Do you mind me telling all my favorite moments?

    (Without looking up from his guitar playing)
    DAD: No, just get it over with.
|]

commentary0 = [r|commentary
```
Fenced

text area
Fenced
Fenced
Fenced
```
|]

commentary1 = [r|commentary
```Fenced

text area
Fenced
Fenced
Fenced
```
|]

-- succeed
hws0 = "53 somatic"
hws1 = "53 somatic, sensate, endue"
hws2 = "somatic, sensate, endue"
hws3 = "45somatic, sensate" -- this passes; should it?
inline0 = [r|some headword "'" allowed! : multi-line
  meaning 
  as long
    as the indentation

  

  is greater than or equal to that of the first line

|]


terminatedTextBlock = [r|    Note: "decency" works better in place of "decorum" than the

    converse. It additionally means sufficient, respectable,
    fairly good, of which meaning "decorum" is without.
    One had better employ "decorum" to describe proper
    --- vs ---
    conformance to propriety, and "decent" (bar its extra
    meaning) to the genuine article, whose outward show of
    decorum and propriety is in no way of artifice but merely
    an unmpremeditated projection of inner material.|]

threeTextBlocks = [r|text block 1
and more
--- vs ---
text block 2

and some more
--- vs ---
text block 3
la di da
|]
defVersus0 = [r|dvs decorum: outward grace or suitableness of conduct, subscription 
    to propriety
    --- vs ---
    decency: having the quality of being moderate,
    seemly, becoming; free from immodesty or obscenity;
    proper formality

    Note: "decency" works better in place of "decorum" than the
    converse. It additionally means sufficient, respectable,
    fairly good, of which meaning "decorum" is without.
    One had better employ "decorum" to describe proper
    conformance to propriety, and "decent" (bar its extra
    meaning) to the genuine article, whose outward show of
    decorum and propriety is in no way of artifice but merely
    an unmpremeditated projection of inner material.

    tl;dr; "decency" has a (positive) moral connotation

|]
