# muse

## What is this?

tl;dr: a reading log parser and search interface

A nascent attempt to solve what problems arise from defining lots of words,
forgetting them but not their context, and then becoming rather miffed at
knowing that one knows a word, when one knew the word, the first letter of the
word, and, possibly, the author or title of the book which contained the word,
and being *still* unable to find said word. muse attempts to solve this by
(a) making more profitable the keeping of a reading log and (b) providing a
still buggy but otherwise helpful--and extremely colorful--search interface to
relieve a vocabulary's growing pains.

## Installation 
Depends on [stack](https://docs.haskellstack.org/en/stable/README/). (Built with version
1.7.1)

Clone; build; and initialize config, cache, and log directory.
```bash
git clone https://gitlab.com/metaporia/muse.git
cd muse
git checkout <release-tag> # e.g., v0.1.0

stack build --copy-bins --ghc-options -O2
# or omit `--copy-bins` and link/copy yourself 

muse init # creates ~/.muse/{entries/,config.yaml}, ~/.cache/muse/parsedEntries/, ~/.muse/state/
# N.B. the migration from manual filesystem-based log-caching is incomplete and
# so both caches are currently in use: expect ~/.cach/muse/parsedEntries to be
# deprecated soon
```

## Log

Log files should be named with the current date, as 'YY.MM.DD'.

For a correctly parsed log file see the [examples](./examples/18.11.24).

I use the following command to log (alter to match chosen log dir):
```bash
#!/usr/bin/env bash
nvim +:last -- ~/.muse/entries/{*,`today`}
```

Additionally, I recommend using the [muse-vim](https://gitlab/metaporia/muse-vim) plugin which provides the above with options to configure the log director and keymap prefix.

If yet another plugin has no appeal to you, consider an adaptation of the following from my (former) .vimrc/[init.vim](https://gitlab.com/metaporia/dot/blob/0578fba492d6da37e2f5a97a325ead9715fe3072/nvim/init.vim):

```vim
au BufEnter ~/sputum/muse/* setfiletype muse

" sets errorformat (WIP) so muse errors can be viewed in vim's quickfix
" window
au BufEnter ~/sputum/muse/* set efm=%EFile:\ %f,%+C>\ (interactive):l:%c:%m,%+Z>\ %.%#,%+C>\ %.%#

" generates a timestamp, e.g., '15:25:52 λ. '
nnoremap <leader>t Go<C-r>=strftime("%H:%M:%S λ. ")<CR>

" inserts separator; see definition comparison. 
au BufEnter ~/sputum/muse/* nnoremap <buffer> <leader>v o<Esc>16i <Esc>a--- vs ---<Esc>o

function! MuseLogEntry()
    execute "normal Go\<C-r>=strftime(\"%H:%M:%S λ. \")\<CR>"
    call feedkeys('A', ' ')
endfunction

function! MuseLastRead()
    call MuseLogEntry()
    call feedkeys(system("muse lastRead --suppress-newline") . "\<ESC>") 
endfunction

command! LogEntry :call LogEntry()
command! LastRead call MuseLastRead()
"continue reading
nnoremap <leader>cr :LastRead<CR>
```

## Parse

After writing logs to ~/.muse/entries, parse them:
```bash
muse parse
```

And behold how spectacularly you flubbed things w.r.t. grammar compliance.
More seriously, there are several parser combinators that silently fail--please
report as bugs any such seemingly misguided decisions by the parser. Inclusion
of both the input log and the JSON output and/or any error messages is
appreciated.


Note: I keep all my logs in a separate repository and sym-link them into
~/.muse/entries lest they suffer from muse's jiggering in an undesirable
fashion&mdash;which they almost certainly will not, but reason so little allays
this superstitios conviction that I continue to do so.



## Search Syntax

Note that this is merely (a potentially outdated version of) the CLI's help information.

```
Usage: muse search [-a|--author ARG] [-t|--title ARG] [-d|--definitions]
                   [-p|--phrases] [-q|--quotes]
                   [--dia|--dial|--dialogues|--dias] [--cmts|--comments]
                   [--dmps|--dumps] [-s|--since REL_DATE]
                   [--def-headword|--dh ARG] [--def-meaning|--dm ARG]
                   [--qb|--quote-body ARG] [-p|--phrases ARG]
                   [--phr-meaning|--pm ARG] [--db|--dialogue-body ARG]
                   [--cb|--comment-body ARG] [--db|--dump-body ARG]
  Search log entries by date, author, title, or predicate on entry contents.
  Inline definitions of headwords or phrases can be searched as well.

Available options:
  -a,--author ARG          Collect entries attributed to satisfactory authors.
  -t,--title ARG           Collect entries attributed to satisfactory authors.
  -d,--definitions         Collect definitions.
  -p,--phrases             Collect phrases.
  -q,--quotes              Collect quotes.
  --dia,--dial,--dialogues,--dias
                           Collect dialogues.
  --cmts,--comments        Collect comments.
  --dmps,--dumps           Collect dumps.
  -s,--since REL_DATE      Lower bound of search filter range
  --def-headword,--dh ARG  Collect defs that satisfy headword search.
  --def-meaning,--dm ARG   Search for strings within meaning/definition.
  --qb,--quote-body ARG    Collect satisfactory quotes.
  -p,--phrases ARG         Collect only satisfactory phrases
  --phr-meaning,--pm ARG   Search for strings within meaning/definition.
  --db,--dialogue-body ARG Collect satisfactory dialogues.
  --cb,--comment-body ARG  Collect satisfactory comments.
  --db,--dump-body ARG     Collect satisfactory dumps.
  -h,--help                Show this help text
```

* A bare invocation of `search` will return all (successfully parsed) entries
  within the last six months.

* Author and title search strings filter quotes--they're applied to quote
  attributions.

* A `REL_DATE` specifies day-month-year offset, e.g., "3d", "3d2m20y",
  "2m5y", and "1d6y". Up to two of the three fields may be omitted. The only
  unsupported order/ommision combination is year-day.

* Search flags, such as `--def-meaning` or  `--author`, may be followed by a
  string for use in an infix search of the target field. Alternatively, many
  such strings may be conjoined, e.g., `--author Edward^Aubyn` would return
  all attributed by an author string matching both "Edward" and "Aubyn".

## Syntax Guidelines

* do not exceed one line in a plural def
* newlines after entry bodies are acceptable
* commas and colons are not permitted in headwords
* add newlines before and after quotation body
* indentation is parsed by tabs made up of four spaces (as of yet
  unconfigurably so)
* avoid comma before a `<read>`'s author attribution--it is, however, permitted
* timestamps without a body are allowed but not encouraged

N.B. `[<expr>]` in an entry example or grammar specification indicates that
`<expr>` is optional; and `(<variant0> | ... | <variantN>)` denotes one of the
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
