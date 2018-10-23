# logParse

## General Rules

* do not exceed one line in a plural def.
* newlines after entry bodies are acceptable
* commas and colons are not permitted in headwords
* add newlines before and after quotation body


### Definition Comparison
```
dvs headword1 : meaning
     --- vs ---
     headword2 : meaning" ## log grammar
```

### List of Headwords
```
d word1, ..., wordN
```

### Inline Definition
```
d headword : meaning

```

### Quotation
```
quotation

"<body>"

<attribution>
```

### Read
Either:

```
read <title>, by <author>
```
or,

```
begin to read <title>, by <author>
```

### Commentary

Stores synthesis of, or commentary on, some piece of text, or merely the
surrounding definitions, quotations, etc.

Parses an commentary entry (body, without timestamp) of the form:
```
"(commentary | synthesis)

 <content>"
```

### Grammar (incomplete)

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
```
