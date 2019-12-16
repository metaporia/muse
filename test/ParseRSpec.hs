{-# LANGUAGE QuasiQuotes #-}

module ParseRSpec where

import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.State            ( State
                                                , runState
                                                , gets
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , maybe
                                                )
import           ParseR                  hiding ( pt
                                                , curr
                                                )
import           Parse.Types                    ( Entry(..)
                                                , DefQuery(..)
                                                , TimeStamp(..)
                                                )
import           Prelude                 hiding ( read
                                                , min
                                                )
import           Test.Hspec
import           Text.Megaparsec.Char           ( newline
                                                , char
                                                )
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Debug          ( dbg )
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )

test = hspec spec 
pt'' = flip parse ""
pt p = fst . flip runState (0, Nothing) . runParserT p "" 

curr''' = do 
  log <- readFile "examples/globLogR"
  pt' logEntries log

tupleRest p = (,) <$> p <*> many anySingle

curr'' = pt' (tupleRest p) multiInlineDefs
 where
  p' = count 2 $ indentedTextBlockUntil $ Just (defVersusSeparator <* newline)
  p = count 2 defVersus' 
  p'' = pt' (inlineMeaning True)  "potter: one who makes pots; to trifle; to walk lazily "

  rest = "potter: one who makes pots; to trifle; to walk lazily "


curr = pt' logEntry dvs'

multiInlineDefs = [r|putter: one who puts; to potter
            --- vs ---
            potter: one who makes pots; to trifle; to walk lazily |]


dvs' = [r|19:29:05 λ. dvs putter: one who puts; to potter
            --- vs ---
            potter: one who makes pots; to trifle; to walk lazily |]

globLog = do log <- readFile "examples/globLogR"
             pt' logEntries log

indentedUntilTest = pt'
  (tupleRest p)
  "    19:31:02 \955. q\n\n        \"...\8212oh, don't go in for accuracy at this house. We all exaggerate, and\n        we get very angry at people who don't.\" \n"
 where
  p = do
    updateIndentation 16 
    indentedTextBlockUntil'

excerpt = [r|
18:06:31 λ. read "A Room with a View" by E.M. Forster
    18:06:32 λ. d disject
    18:38:08 λ. d choric: of, by, or relating to a chorus
    18:58:19 λ. d flurry (v.)
    19:00:14 λ. d shibboleth
    19:07:00 λ. d twaddle, twattle
    19:29:05 λ. dvs putter: one who puts; to potter
                --- vs ---
                potter: one who makes pots; to trifle; to walk lazily 
    19:31:02 λ. q

        "...—oh, don't go in for accuracy at this house. We all exaggerate, and
        we get very angry at people who don't." 
|]

defs = [r|22:03:09 λ. d dour
21:28:41 λ. d irremunerable : beyond compensation
22:31:29 λ. d harlequin
23:32:36 λ. d impetuous
19:07:00 λ. d twaddle, twattle
    19:29:05 λ. dvs putter: one who puts; to potter
                --- vs ---
                potter: one who makes pots; to trifle; to walk lazily 
|]

curr' = pt' (manyTill anySingle (satisfy (== '\n')) :: Parser String)  "aeou \n \n"



spec :: Spec
spec = do
  describe "logEntries" $ do
    it "defVersus greed-check" $ pt logEntries excerpt `shouldParse` excerptOut
    it "examples/globLogR" $ do
      log <- liftIO $ readFile "examples/globLogR"
      pt logEntries log `shouldParse` globLogR
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
    it "indented text block" $ do
      pt indentedTextBlock indentedV0
        `shouldParse` ("this\nis\na\n\n  \n\nindented\ntext\nblock", 2)
      let p = do
            updateIndentation 16
            indentedTextBlockUntil'
      pt (tupleRest p)
        `shouldFailOn` "    19:31:02 \955. q\n\n        \"...\8212oh, don't go in for accuracy at this house. We all exaggerate, and\n        we get very angry at people who don't.\" \n"
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
    it "inline, single line, newline"
      $             pt (inlineMeaning False) "hello : world\n"
      `shouldParse` ("hello", "world") -- FIXME trim trailing whitespace
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
      $             pt logEntries defVersus0
      `shouldParse` [ ( 0
                      , TimeStamp 19 29 05
                      , Def
                        (DefVersus
                          "decorum"
                          "outward grace or suitableness of conduct, subscription \nto propriety\n"
                          "decency"
                          "having the quality of being moderate,\nseemly, becoming; free from immodesty or obscenity;\nproper formality\n\nNote: \"decency\" works better in place of \"decorum\" than the\nconverse. It additionally means sufficient, respectable,\nfairly good, of which meaning \"decorum\" is without.\nOne had better employ \"decorum\" to describe proper\nconformance to propriety, and \"decent\" (bar its extra\nmeaning) to the genuine article, whose outward show of\ndecorum and propriety is in no way of artifice but merely\nan unmpremeditated projection of inner material.\n\ntl;dr; \"decency\" has a (positive) moral connotation"
                        )
                      )
                    , ( 0
                      , TimeStamp 8 47 47
                      , Read "The Mill on the Floss"
                             "George Eliot (Mary Ann Evans)"
                      )
                    ]
    it "defVersus: two single-line meanings, no trailing line"
      $             pt logEntry dvsTrailing
      `shouldParse` ( 0
                    , TimeStamp {hr = 19, min = 29, sec = 5}
                    , Def
                      (DefVersus
                        "putter"
                        "one who puts; to potter"
                        "potter"
                        "one who makes pots; to trifle; to walk lazily"
                      )
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

defVersus0 = [r|19:29:05 λ. dvs decorum: outward grace or suitableness of conduct, subscription 
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

08:47:47 λ. read "The Mill on the Floss" by George Eliot (Mary Ann Evans)
|]

dvsTrailing = [r|19:29:05 λ. dvs putter: one who puts; to potter
            --- vs ---
            potter: one who makes pots; to trifle; to walk lazily
|]

logs = [r|
08:47:47 λ. read "The Mill on the Floss" by George Eliot (Mary Ann Evans)
08:47:48 λ. q
    
    "What novelty is worth that sweet monotony where everything is known,
    and _loved_ because it is known?"

08:48:52 λ. q

    "...that fly-fishers fail in preparing their bait so as to make it 
    alluring in the right quarter, for want of a due acquaintance with 
    the subjectivity of fishes."

08:50:04 λ. d brimful 
|]

logsIndented = [r|
08:47:47 λ. read "The Mill on the Floss" by George Eliot (Mary Ann Evans)
    08:47:48 λ. q
        
        "What novelty is worth that sweet monotony where everything is known,
        and _loved_ because it is known?"

    08:48:52 λ. q

        "...that fly-fishers fail in preparing their bait so as to make it 
        alluring in the right quarter, for want of a due acquaintance with 
        the subjectivity of fishes."

    08:50:04 λ. d brimful

08:50:05 λ. read "Guards! Guards!" by Terry Pratchett
    

|]

excerptOut =
  [ ( 0
    , TimeStamp {hr = 18, min = 6, sec = 31}
    , Read "A Room with a View" "E.M. Forster"
    )
  , (4, TimeStamp {hr = 18, min = 6, sec = 32}, Def (Defn Nothing ["disject"]))
  , ( 4
    , TimeStamp {hr = 18, min = 38, sec = 8}
    , Def (InlineDef "choric" "of, by, or relating to a chorus")
    )
  , ( 4
    , TimeStamp {hr = 18, min = 58, sec = 19}
    , Def (Defn Nothing ["flurry (v.)"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 0, sec = 14}
    , Def (Defn Nothing ["shibboleth"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 7, sec = 0}
    , Def (Defn Nothing ["twaddle", "twattle"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 29, sec = 5}
    , Def
      (DefVersus "putter"
                 "one who puts; to potter"
                 "potter"
                 "one who makes pots; to trifle; to walk lazily "
      )
    )
  , ( 4
    , TimeStamp {hr = 19, min = 31, sec = 2}
    , Quotation
      "...\8212oh, don't go in for accuracy at this house. We all exaggerate, and\nwe get very angry at people who don't."
      ""
      Nothing
    )
  ]

globLogR =
  [ ( 0
    , TimeStamp {hr = 8, min = 47, sec = 47}
    , Read "The Mill on the Floss" "George Eliot (Mary Ann Evans)"
    )
  , ( 4
    , TimeStamp {hr = 8, min = 47, sec = 48}
    , Quotation
      "What novelty is worth that sweet monotony where everything is known,\nand _loved_ because it is known?"
      ""
      Nothing
    )
  , ( 4
    , TimeStamp {hr = 8, min = 48, sec = 52}
    , Quotation
      "...that fly-fishers fail in preparing their bait so as to make it \nalluring in the right quarter, for want of a due acquaintance with \nthe subjectivity of fishes."
      ""
      Nothing
    )
  , (4, TimeStamp {hr = 8, min = 50, sec = 4}, Def (Defn Nothing ["brimful"]))
  , ( 4
    , TimeStamp {hr = 8, min = 50, sec = 29}
    , Def (Defn Nothing ["\"rapt in\""])
    )
  , (4, TimeStamp {hr = 8, min = 51, sec = 24}, Def (Defn Nothing ["trebly"]))
  , ( 4
    , TimeStamp {hr = 8, min = 51, sec = 33}
    , Def (Defn Nothing ["sanguinary"])
    )
  , ( 4
    , TimeStamp {hr = 8, min = 51, sec = 39}
    , Def (Defn Nothing ["topsy-turvy"])
    )
  , ( 4
    , TimeStamp {hr = 8, min = 51, sec = 44}
    , Def (Defn Nothing ["\"omit to\"", "\"fail of\""])
    )
  , (4, TimeStamp {hr = 8, min = 51, sec = 58}, Def (Defn Nothing ["tonic"]))
  , ( 4
    , TimeStamp {hr = 8, min = 52, sec = 0}
    , Def (InlineDef "bowl" "(v.) to move along")
    )
  , ( 4
    , TimeStamp {hr = 8, min = 52, sec = 25}
    , Def (InlineDef "stodgy" "dull, old-fashioned")
    )
  , (4, TimeStamp {hr = 8, min = 53, sec = 6} , Def (Defn Nothing ["merest"]))
  , (4, TimeStamp {hr = 8, min = 53, sec = 11}, Def (Defn Nothing ["fortuity"]))
  , ( 4
    , TimeStamp {hr = 8, min = 53, sec = 16}
    , Def (Defn Nothing ["\"be hindered of\""])
    )
  , ( 4
    , TimeStamp {hr = 8, min = 53, sec = 38}
    , Def
      (InlineDef "pother" "(n.) bustle, tumult, bother; (v.) to perplex, worry")
    )
  , ( 0
    , TimeStamp {hr = 12, min = 13, sec = 16}
    , Read "Wings of the Dove" "Henry James"
    )
  , ( 4
    , TimeStamp {hr = 12, min = 19, sec = 51}
    , Quotation "Lucid and ironic, she knew no merciful muddle." "" Nothing
    )
  , ( 4
    , TimeStamp {hr = 12, min = 29, sec = 25}
    , Def (Defn Nothing ["malachite"])
    )
  , ( 4
    , TimeStamp {hr = 12, min = 39, sec = 46}
    , Def (Defn Nothing ["salutary terror"])
    )
  , ( 4
    , TimeStamp {hr = 12, min = 41, sec = 28}
    , Def
      (InlineDef
        "\"in view of\""
        "as regards the accord of a thing to some other \nobject, upcoming event, or circumstance"
      )
    )
  , ( 4
    , TimeStamp {hr = 12, min = 45, sec = 24}
    , Def (InlineDef "patrimony" "an inheritance from one's father")
    )
  , ( 0
    , TimeStamp {hr = 17, min = 15, sec = 55}
    , Read "Jitterbug Perfume" "Tom Robbins"
    )
  , ( 4
    , TimeStamp {hr = 17, min = 33, sec = 59}
    , Commentary "<Insightful lexical ejaculate /here/>"
    )
  , ( 4
    , TimeStamp {hr = 17, min = 51, sec = 1}
    , Quotation "You misunderstand me. I do not fear death. I _resent_ it."
                ""
                Nothing
    )
  , ( 0
    , TimeStamp {hr = 17, min = 52, sec = 16}
    , Read "Northanger Abbey" "Jane Austen"
    )
  , ( 4
    , TimeStamp {hr = 17, min = 52, sec = 17}
    , Def (InlineDef "mizzle" "a misty drizzle")
    )
  , ( 4
    , TimeStamp {hr = 17, min = 52, sec = 49}
    , Quotation
      "Miss Morland, no one can think more highly of the understanding of\nwomen than I do. In my opinion, nature has given them so much, that\nthey never find it necessary to use more than half."
      ""
      Nothing
    )
  , ( 4
    , TimeStamp {hr = 17, min = 52, sec = 50}
    , Commentary "<A second jettisoned insight /here/>"
    )
  , ( 0
    , TimeStamp {hr = 18, min = 6, sec = 31}
    , Read "A Room with a View" "E.M. Forster"
    )
  , (4, TimeStamp {hr = 18, min = 6, sec = 32}, Def (Defn Nothing ["disject"]))
  , ( 4
    , TimeStamp {hr = 18, min = 38, sec = 8}
    , Def (InlineDef "choric" "of, by, or relating to a chorus")
    )
  , ( 4
    , TimeStamp {hr = 18, min = 58, sec = 19}
    , Def (Defn Nothing ["flurry (v.)"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 0, sec = 14}
    , Def (Defn Nothing ["shibboleth"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 7, sec = 0}
    , Def (Defn Nothing ["twaddle", "twattle"])
    )
  , ( 4
    , TimeStamp {hr = 19, min = 29, sec = 5}
    , Def
      (DefVersus "putter"
                 "one who puts; to potter"
                 "potter"
                 "one who makes pots; to trifle; to walk lazily "
      )
    )
  , ( 4
    , TimeStamp {hr = 19, min = 31, sec = 2}
    , Quotation
      "...\8212oh, don't go in for accuracy at this house. We all exaggerate, and\nwe get very angry at people who don't."
      ""
      Nothing
    )
  , ( 4
    , TimeStamp {hr = 19, min = 40, sec = 45}
    , Def
      (InlineDef
        "prig"
        "to haggle over a price; to steal; a thief; a conceited moralist"
      )
    )
  , ( 4
    , TimeStamp {hr = 20, min = 49, sec = 24}
    , Quotation "...he had shown her the holiness of direct desire." "" Nothing
    )
  , ( 0
    , TimeStamp {hr = 21, min = 28, sec = 18}
    , Quotation
      "{There's no,What} better antidote to respect than hypocrisy{.,?}"
      "Keane Yahn-Krafft"
      Nothing
    )
  , ( 0
    , TimeStamp {hr = 21, min = 28, sec = 41}
    , Def (InlineDef "irremunerable" "beyond compensation")
    )
  , (0, TimeStamp {hr = 21, min = 43, sec = 44}, Read "Emma" "Jane Austen")
  , ( 4
    , TimeStamp {hr = 21, min = 44, sec = 21}
    , Quotation "Better be without sense, than misapply it as you do."
                ""
                Nothing
    )
  , (0, TimeStamp {hr = 22, min = 3, sec = 9}, Def (Defn Nothing ["dour"]))
  , ( 0
    , TimeStamp {hr = 22, min = 31, sec = 29}
    , Def (Defn Nothing ["harlequin"])
    )
  , ( 0
    , TimeStamp {hr = 23, min = 32, sec = 36}
    , Def (Defn Nothing ["impetuous"])
    )
  , ( 0
    , TimeStamp {hr = 23, min = 33, sec = 42}
    , Quotation
      "Rationalization isn't just a river in Egyt\8212wait, that's denial."
      "IZombie"
      Nothing
    )
  ]
