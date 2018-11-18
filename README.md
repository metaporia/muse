# muse

## What is this?

tl;dr: a reading log parser and search interface

A nascent attempt to solve the what problems arise from defining lots of words,
forgetting them but not their context, and then becoming rather miffed at
knowing that one knows a word, when one knew the word, the first letter of the
word, and, possibly, the author or title of the book which contained the word,
and being *still* unable to find said word. muse attempts to solve this by
(a) making more profitable the keeping of a reading log and (b) providing a
still buggy but otherwise helpful--and extremely colorful--search interface to
relieve a vocabulary's growing pains.

## Installation 
Depends on [stack](https://docs.haskellstack.org/en/stable/README/). (Built with version
1.71.)

Clone, build and initialize config, cache, and log directory.
```bash
git clone https://gitlab.com/metaporia/muse.git
cd muse
git checkout <release-tag> # e.g., v0.1.0
stack build --copy-bins # or omit `--copy-bins` and link/copy yourself 
muse init # creates ~/.muse/{entries/,config.yaml}, ~/.cache/muse/parsedEntries/, 
```

## Log

I use the following command to log (with nvim):
```bash
#!/usr/bin/env bash
nvim +:last -- ~/.muse/entries/{*,`today`}
```

Additionally, I have the following bindings in my .vimrc/init.vim:
```vim
" create filetype trigger for bindings
au BufEnter ~/.muse/entries/* setfiletype muse

" generates a timestamp, e.g., '15:25:52 λ. '
au FileType muse nnoremap <buffer> <leader>t Go<C-r>=strftime("%H:%M:%S λ. ")<CR>

" inserts separator; see definition comparison. 
au FileType muse nnoremap <buffer> <leader>v o<Esc>16i <Esc>a--- vs ---<Esc>o

" sets errorformat (WIP) so muse errors can be viewed in vim's quickfix
" window
au FileType muse setlocal efm=%EFile:\ %f,%+C>\ (interactive):l:%c:%m,%+Z>\ %.%#,%+C>\ %.%#
```

## Parse

After writing logs to ~/.muse/entries, parse them:
```bash
muse parse --all --ignore-cache
```

And behold how spectacularly you flubbed things w.r.t. grammar compliance.
More seriously, there are several parser combinators that silently fail--please
report as bugs any such seemingly misguided decisions by the parser. Inclusion
of both the input log and the JSON output and/or any error messages is
appreciated.


## Search Syntax

Note that this is merely (a potentially outdated) the CLI's help information.

```
Usage: muse search [-w|--within REL_DATE] [-a|--author SUBSTR]
                           [-t|--title SUBSTR] [-d|--definitions]
                           [-q|--quotations] [-p|--phrases] [-l|--dialogues]

  Search log entries by date, author, or title; extract definitions or
  quotations.

Available options:
  -w,--within REL_DATE     Lower bound of search filter range
  -a,--author SUBSTR       Substring/affix of author
  -t,--title SUBSTR        Affix of title
  -d,--definitions         Collect only definitions
  -q,--quotations          Collect only quotations
  -p,--phrases             Collect only phrases
  -l,--dialogues           Collect only dialogue
  -h,--help                Show this help text
```

* A bare invocation of `search` will return all (successfully parsed) entries
  within the last six months.

* Author and title search strings filter quotes--they're applied to quote
  attributions.

* Author and title search strings can be prefixed by a space proceeding one of
  the following search modes:
   - 'p': prefix search
   - 'i': infix search, the default
   - 's': suffix search

  For example, `--author 's Woolf'` searches for entries whose author
  attribution ends with the string "Woolf"

* A `REL_DATE` specifies day-month-year offset, e.g., "3d", "3d2m20y",
  "2m5y", and "1d6y". Up to two of the three fields may be omited, but they
  must remain in the correct order, namely, day-month-year. A relative date is
  represented by a a number of days, months, and years, each of which is
  subtracted from the date of invocation to generate the lower bound of the
  date range of searched entries.


## Syntax Guidelines

* do not exceed one line in a plural def
* newlines after entry bodies are acceptable
* commas and colons are not permitted in headwords
* add newlines before and after quotation body
* indentation is parsed by tabs made up of four spaces (as of yet
  unconfigurably so)
* avoid comma before a <read>'s author attribution--it is, however, permitted
* timestamps without a body are allowed but not encouraged

N.B. '[<expr>]' in an entry example or grammar specification indicates that
<expr> is optional; and '(<variant0> | ... | <variantN>)' denotes one of the
listed options must be selected, but that any one of which is accepted.

### Definition Comparison
```
dvs <headword>[ ]: <meaning>
     --- vs ---
    <headword>[ ]: <meaning>" 
```

Wherein the space preceding the colon is optional.

### List of Headwords
```
d [<pg-num>] word1, ..., wordN
```

N.B. may not exceed one line/contain newline (the parser consumes
comma-separated headwords until a newline).


### Inline Definition
```
d headword : meaning

```
As in a definition comparison, the space before the colon is optional.

### Quotation
```
quotation [<page-num>]

"<body>"

<attribution>
```

A `<page-num>` consists of arbitrarily many digits.

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

The comma preceding the author attribution is optional, reflects whether you,
as the logger, wish the attribution to be included in a restrictive or
non-restrictive clause. Since there is a slight difference in meaning between
the two--namely, that the former means, "of the (many) books read by
`<author>`, the one by the name of `<title>` is read", whereas the latter
means, "a book by the name of `<title>` is read, which happens to be by
`<author>`"--, I leave the choice to the user.


### Commentary

Stores synthesis of, or commentary on, some piece of text, or merely the
surrounding definitions, quotations, etc.

Parses an commentary entry (body, without timestamp) of the form:
```
"(commentary | synthesis)

 <content>"
```

N.B. `<content>` may span multiple lines, but cannot contain a `<timeStamp>`,
as it is used as a closing delimiter.

### Dump
As yet devoid of internal formatting, a `<dump>` comprises zero or more lines
and may not contain an ellipsis followed by a newline, e.g.,

```
...
<optionally-multi-line-dump-body>
...
```

### Phrase 

Log memorable collocations with the following:

```
(phrase | phr) <phrase>[[ ]: <meaning>]
```

Wherein both the meaning and the space preceding the colon are optional.

Both "phrase" and "phr" are valid prefixes.


### Dialogue

Although a `<dialogue>` entry body does not, at the moment, impose by parser
any content restrictions, my intent is that dialogue conform to the following
general form:
```
dialogue

    <char>: <line>

    {"\n\n" <char>: <line> }
```
In other words, dialogue consists of newline separated paragraphs headed by the
speaking character's name followed by a colon and the contents of the line,
which may occupy multiple lines of text.


### Beware the incomplete grammar (seriously, read no farther)

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
