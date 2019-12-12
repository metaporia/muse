{-# LANGUAGE QuasiQuotes #-}

module ParseRSpec where

import           ParseR                  hiding ( pt, curr )
import           Parse.Types                    ( Entry(..) )
import           Test.Hspec
import           Text.Megaparsec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )

test = hspec spec 
pt = flip parse ""

spec :: Spec
spec = do
  describe "textBlock" $ do
    it "indented text block" $ pt indentedTextBlock indentedV0 `shouldBe` Right
      ("this\nis\na\n\n  \n\nindented\ntext\nblock", 2)
    it "fenced text block" $ do
      pt fencedTextBlock fenceV0
        `shouldBe` Right "text block\n\nof \n\nmany \n\n\n\nlines"
      pt fencedTextBlock (fenceV0 ++ "  \n \n\n\n  \n")
        `shouldBe` Right "text block\n\nof \n\nmany \n\n\n\nlines"
      pt fencedTextBlock fenceV1
        `shouldBe` Right "text block\n\nof \n\nmany \n\n  \n\nlines"
    it "commentary" $ do
      pt commentary commentary0 `shouldBe` Right
        (Commentary "Fenced\n\ntext area\nFenced\nFenced\nFenced\n")
      pt commentary commentary1 `shouldBe` Right
        (Commentary "Fenced\n\ntext area\nFenced\nFenced\nFenced\n")
    it "dialogue" $ do
      pt dialogue dialogue0 `shouldBe` Right
        (Dialogue
          "(After ~1hr of unbridled loquacity, having mercifully dammed the torrent)\nMOM: Do you mind me telling all my favorite moments?\n\n(Without looking up from his guitar playing)\nDAD: No, just get it over with."
        )
  describe "emptyLine"
    $          it "skip newlines and lines with spaces"
    $          pt (many (try emptyLine)) emptyLineEx
    `shouldBe` Right
                 [ "\n"
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
      $          pt quotation multiSkipNoAttrPg
      `shouldBe` Right (Quotation "quote \n\nk\n\nbody \ncont." "" (Just 23))
    it "multiline, no skip, no attr"
      $          pt quotation multiNoSkipNoAttrNoPg
      `shouldBe` Right (Quotation "quote \nk\nbody \ncont." "" Nothing)
    it "multiline, skip, attr, no pg"
      $          pt quotation multiSkipAttrNoPg
      `shouldBe` Right (Quotation "quote\n\nbody\ncont." "attr" Nothing)
    it "quote content, multi, skip"
      $          pt quoteContent qcMultiSkipTrim
      `shouldBe` Right
                   ( "this is the\nquote.\n\n\n\n   \n\n\n\n       \n\nSpanning multiple\nlines."
                   , 4
                   )

curr = pPrint $ pt dialogue dialogue0

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
