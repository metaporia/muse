# logParse

## log grammar

### BNF

```

<entry> ::= <timeStamp> <def>

<timeStamp> ::= 2DIGIT ":" 2DIGIT ":" 2DIGIT " λ. "

<def> ::= <defPlural> | <defSingle> | <defInline> | <DefVs>

    <defPlural> ::= <headword> { ", " <headword> } // one + zero or more
    <defSingle> ::= <headword>
    <defInline> ::= <headword> " : " <meaning>
    <DefVs>     ::= <headword> " : " <meaning'> 
                    "--- vs ---" 
                    <headword> " : " <meaning>

    <headword> ::=  [^:]*
    <meaning>  ::= (.*)
    <meaning'> ::= .+?(--- vs ---) // i.e. match as few characters as possible
                    // until "--- vs ---" is found.
                    // E.g., `manyTill anyChar (try (string "--- vs ---"))`

<read> ::= ("begin to read " | "read ") <title> ", by " <author>
    
    <title> ::= "[^"]*"
    <author> ::= <toNextTimeStampOrEOF>

<toNextTimeStampOrEOF> ::= `^(*)?<timestamp> // e.g. `manyTill anyChar (try timestamp)```

