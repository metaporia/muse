{-# LANGUAGE QuasiQuotes #-}

module ParseRSpec where

import           ParseR                         ( quotation, emptyLine , quoteContent)
import           Test.Hspec
import           Text.Megaparsec
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )

test = hspec spec 
pt = flip parse ""

spec :: Spec
spec = do
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
      `shouldBe` Right (Just 23, "quote \n\nk\n\nbody \ncont.", Nothing)
    it "multiline, no skip, no attr"
      $          pt quotation multiNoSkipNoAttrNoPg
      `shouldBe` Right (Nothing, "quote \nk\nbody \ncont.", Nothing)
    it "multiline, skip, attr, no pg"
      $          pt quotation multiSkipAttrNoPg
      `shouldBe` Right (Nothing, "quote\n\nbody\ncont.", Just "attr")
    it "quote content, multi, skip"
      $          pt quoteContent qcMultiSkipTrim
      `shouldBe` Right
                   ( "this is the\nquote.\n\n\n\n   \n\n\n\n       \n\nSpanning multiple\nlines."
                   , 4
                   )

curr = pPrint $ pt quoteContent qcMultiSkipTrim


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


