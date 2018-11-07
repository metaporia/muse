# logParse

## General Rules

* do not exceed one line in a plural def
* newlines after entry bodies are acceptable
* commas and colons are not permitted in headwords
* add newlines before and after quotation body
* indentation is parsed by tabs made up of four spaces (as of yet
  unconfigurably so)
* avoid comma before a <read>'s author attribution--it is, however, permitted
* timestamps without a body are allowed but not encouraged

N.B. '[<expr>]' in an entry example or grammar specification indicates that
<expr> is optional

### Definition Comparison
```
dvs headword1 : meaning
     --- vs ---
     headword2 : meaning" ## log grammar
```

### List of Headwords
```
d [<pg-num>] word1, ..., wordN
```

### Inline Definition
```
d headword : meaning

```

### Quotation
```
quotation [<page-num>]

"<body>"

<attribution>
```

### Read
One of:

```
read <title>[,] by <author>
```

```
begin to read <title>[,] by <author>
```

```
finsh [reading] <title>[,] by <author>
```



### Commentary

Stores synthesis of, or commentary on, some piece of text, or merely the
surrounding definitions, quotations, etc.

Parses an commentary entry (body, without timestamp) of the form:
```
"(commentary | synthesis)

 <content>"
```

### Dump
As yet devoid of internal formatting, e.g.:

```
...
<optionally-multi-line-dump-body>
...
```

### Beware the incomplete grammar 

This lags behind the implementation; for up-to-date information on the grammar
see ./src/Parse.hs (for the implementation) and ./test/Spec.hs (for tests).

```
<entry> ::= <timeStamp> (<def> | <read> | <dump> | <quotation>)

<timeStamp> ::= 2DIGIT ":" 2DIGIT ":" 2DIGIT " λ. "

<def> ::= <defPlural> | <defSingle> | <defInline> | <DefVs>

    <defPlural> ::= [<page-num>] <headword> { ", " <headword> } // one + zero or more
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
```
