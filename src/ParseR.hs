{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ParseR
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Rewrite of 'Parse' and 'Parse.Entry'.
-- Contains non-greedy entry body parsers.
--
-----------------------------------------------------------------------------
module ParseR where


import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad                  ( void
                                                , unless
                                                , when
                                                )
import           Control.Monad.State            ( State
                                                , runState
                                                , modify
                                                , gets
                                                )
import           Data.Char                      ( digitToInt )
import           Data.Maybe                     ( fromMaybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Void                      ( Void )
import           Parse.Types                    ( Entry(..)
                                                , DefQuery(..)
                                                , LogEntry(..)
                                                , PageNum(..)
                                                , Phrase(..)
                                                , TimeStamp(..)
                                                )
import           Prelude                 hiding ( read )
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import           Text.Megaparsec.Debug          ( dbg )
import           Debug.Trace                    ( trace )

---------------
-- LOG ENTRY --
---------------


logEntries = many (try emptyLine) *> some logEntry <* eof

logEntry :: Parser (Int, TimeStamp, Entry)
logEntry = entry <* many (try emptyLine)
 where
  entry = do
    indentLevel <- sum <$> many (1 <$ satisfy (== ' ')) <?> "leading whitespace"
    ts          <- timestamp
    --dbg ("updateIndentation " <> show indentLevel) $
    updateIndentation indentLevel
    entry <-
      try read
      <|> try quotation
      <|> try definition
      <|> try commentary
      <|> try pageNum
      <|> try phrase
      <|> dialogue
    --dbg (show indentLevel <> ": " <> show ts) (pure ())
    updateTimeStamp ts
    return (indentLevel, ts, entry)

updateTimeStamp :: TimeStamp -> Parser ()
updateTimeStamp ts = modify $ \(a, _) -> (a, Just ts)

updateIndentation :: Int -> Parser ()
updateIndentation i = --dbg ("updateidnt " <> show i) $
  modify $ \(_, ts) -> (i, ts)


---------------------------
-- Commentary & Dialogue --
---------------------------

-- TODO add multiline string literal syntax, e.g., use ``` to delimit raw
-- strings. Otherwise, the commentary must be indented, which messes up the
-- line wrap.
--
-- FIXME indentation-sensitive comment parsing will break, e.g., that
-- inline-markdown comment on /Carry On, Jeeves/.
commentaryPrefix :: Parser String
commentaryPrefix = cp <?> "commentary prefix"
  where cp = try (symbol "commentary") <|> symbol "synthesis"

commentary :: Parser Entry
commentary = textBlockEntry commentaryPrefix Commentary <?> "commentary"

dialogue :: Parser Entry
dialogue = textBlockEntry dialoguePrefix Dialogue <?> "dialogue"

dialoguePrefix :: Parser String
dialoguePrefix = cp <?> "dialogue prefix" where cp = symbol "dialogue"


-- | Parameterizes construction of the two isomorphic 'Entry' variants,
-- 'Commentary' and 'Dialogue'.
textBlockEntry :: Parser String -> (String -> Entry) -> Parser Entry
textBlockEntry prefix toEntry = do
  prefix
  many (try emptyLine)
  toEntry <$> textBlock


-- | Parse a text block. A text block may be either indented more than its
-- parent timestamp or fenced with triple-backticks (```).
--
-- Note that this is /not/ backwards-compatible and that older log files will
-- have to be corrected. My hope is that they will be rejected by the new
-- parser loudly and with precision, so ensuring compliance  with the new
-- spec shouldn't be too much work.
textBlock = try fencedTextBlock <|> fst <$> indentedTextBlock

-- | Parses indented blocks of text. It will succeed with whatever it has when
-- it first encounters insufficiently indented input, so its well-formedness
-- may be reported as a failure the next applied parser.
--
-- Newlines after the block are discarded.
indentedTextBlock :: Parser (String, Int)
indentedTextBlock = do
  let
    lines indent = do
      line       <- many (satisfy (/= '\n'))
      emptyLines <- concat <$> many (try (indentedEmptyLine indent))
      rest       <-
        try
            (  count indent (satisfy (== ' '))
            *> fmap (emptyLines <>) (lines indent)
            )
          <|> pure ""
      return $ line <> rest
  indentLevel <- sum <$> many (1 <$ satisfy (== ' ')) <?> "leading whitespace"
  minIndent   <- gets fst
  unless (minIndent < indentLevel)
         (fail "indentedTextBlock: insufficient indentation")
  --(  dbg
  --  $  "indentedTextBlock: indent lower bound = "
  --  <> show minIndent
  --  <> " actual indent = "
  --  <> show indentLevel
  --  )
  --  (pure ())
  --dbg ("tb: " <> show indentLevel) (pure ())
  (, indentLevel) <$> lines indentLevel


-- | Like 'indentedTextBlock' but with optional block terminator, @mEnd@, which
-- should expect no leading whitespace and to consume the rest of the line
-- (newline included) but none of the proceeding line.
indentedTextBlockUntil :: Maybe (Parser String) -> Parser (String, Int)
indentedTextBlockUntil Nothing             = indentedTextBlock
indentedTextBlockUntil (Just terminalLine) = do
  let
    lines indent = do
      line <- Nothing <$ terminalLine <|> Just <$> some (satisfy (/= '\n'))
      case line of
        Just ln -> do
          emptyLines <- concat <$> many (try (indentedEmptyLine indent))
          rest       <-
            try
                (  count indent (satisfy (== ' '))
                *> fmap (emptyLines <>) (lines indent)
                )
              <|> pure ""
          return $ ln <> rest
        Nothing -> return ""
  indentLevel <- sum <$> many (1 <$ satisfy (== ' ')) <?> "leading whitespace"
  --updateIndentation indentLevel
  (, indentLevel) <$> lines indentLevel

-- | A triple-backtick-fenced block of raw text. The fences may be placed
-- directly after the entry variant prefix, on a line of their own, or at the
-- start or end a of line; the three forms /should/ be equivalent. In any case, this
-- parser expects the first chunk of its input stream to be a fence.
--
-- If present after the opening fence, a single newline is dropped.
--
-- Newlines after the block are discarded.
fencedTextBlock :: Parser String
fencedTextBlock = do
  let fence = symbol "```"
  fence <* optional newline
  manyTill anySingle fence <* many emptyLine


----------------
-- Definition --
----------------

phrase :: Parser Entry
phrase = do
  let pPrefix = try (symbol "phrase") <|> symbol "phr"
      inline  = pPrefix *> (uncurry Defined <$> inlineMeaning False)
      hws     = pPrefix *> (Plural . snd <$> headwords)
  fmap Phr $ try inline <|> hws

-- TODO use prefix "d" for all def types (blocked on unifying 'InlineDef' and
-- 'DefVersus').
definition :: Parser Entry
definition = label "definition" $ do
  let dvs     = symbol "dvs" *> defVersus'
      dPrefix = try (symbol "definition") <|> try (symbol "def") <|> symbol "d"
      inline  = dPrefix *> (uncurry InlineDef <$> inlineMeaning False)
      hws     = dPrefix *> (uncurry Defn <$> headwords)
  fmap Def $ try dvs <|> try inline <|> hws

-- | Parses a list of comma separated headwords. It must not exceed one line.
headwords :: Parser (Maybe Integer, [String])
headwords = label "headword(s)" $ do
  pg  <- optional (lexeme L.decimal)
  hws <- lexeme $ sepBy1
    (some $ satisfy (\x -> x /= '\n' && x /= ',' && x /= ':'))
    (lexeme (char ','))
  many (try emptyLine)
  return (fromIntegral <$> pg, hws)

-- TODO optional page nmuber
inlineMeaning
  :: Bool -- ^ Whether to for definition comparison separator.
  -> Parser (String, String)
inlineMeaning inDefComparison = label "inlineDef" $ do
  let indentedTextBlock' = if inDefComparison
        then indentedTextBlockUntil $ Just (defVersusSeparator <* newline)
        else indentedTextBlock
  -- TODO parse list of words: sequence of non-space characters separated by
  -- spaces. This would obivate the need to trim trailing whitespace from the
  -- headword.
  headword <- someTill
    (satisfy (\x -> x /= '\n' && x /= ':'))
    (try (satisfy (== ' ') <* lexeme (char ':')) <|> lexeme (char ':'))
  meaningLine1                 <- some (satisfy (/= '\n')) <* newline
  minIndent                    <- gets fst
  (restOfMeaning, indentLevel) <- try indentedTextBlock'
    <|> return ("", minIndent)
  --dbg ("updateIndentation " <> show minIndent) $
  updateIndentation minIndent
  --(  dbg
  --  $  "inlineMeaning: indent lower bound = "
  --  <> show minIndent
  --  <> " actual indent = "
  --  <> show indentLevel
  --  <> "rest="
  --  <> show (show restOfMeaning)
  --  <> "|"
  --  <> show (length restOfMeaning)
  --  <> show (null restOfMeaning)
  --  )
  --  (pure ())
  -- TODO, HERE FIXME figure out correct indentation enforcement !!!
  when
    (not (null restOfMeaning) && minIndent >= indentLevel)
    (  fail
    $  "inlineMeaning: insufficient indentation-"
    <> headword
    <> ":"
    <> meaningLine1
    <> "\n"
    <> "idt="
    <> show indentLevel
    <> ","
    <> show minIndent
    <> "-"
    <> restOfMeaning
    )
  return
    ( headword
    , meaningLine1 ++ if null restOfMeaning then "" else '\n' : restOfMeaning
    )

defVersus' :: Parser DefQuery
defVersus' = do
  (hw , mn ) <- defVersusFirst
  (hw', mn') <- defVersusNext
  return $ DefVersus hw mn hw' mn'

-- | For 'DefVersus'
defVersusFirst :: Parser (String, String)
defVersusFirst = label "defVersusFirst" $ do
  -- TODO parse list of words: sequence of non-space characters separated by
  -- spaces. This would obivate the need to trim trailing whitespace from the
  -- headword.
  headword <- someTill
    (satisfy (\x -> x /= '\n' && x /= ':'))
    (try (satisfy (== ' ') <* lexeme (char ':')) <|> lexeme (char ':'))
  meaningLine1                 <- some (satisfy (/= '\n')) <* newline
  minIndent                    <- gets fst
  (restOfMeaning, indentLevel) <- indentedTextBlockUntil'
    <|> return ("", minIndent)
  --dbg ("dvf updateIndentation " <> show indentLevel) $
  updateIndentation indentLevel
  -- TODO, HERE FIXME figure out correct indentation enforcement !!!
  when
    (not (null restOfMeaning) && minIndent >= indentLevel)
    (  fail
    $  "defVersus': insufficient indentation-"
    <> headword
    <> ":"
    <> meaningLine1
    <> "\n"
    <> "idt="
    <> show indentLevel
    <> ","
    <> show minIndent
    <> "-"
    <> restOfMeaning
    )
  return
    ( headword
    , meaningLine1 ++ if null restOfMeaning then "" else '\n' : restOfMeaning
    )
  --meaning <- _ --

-- | Parse the next comparison.
defVersusNext :: Parser (String, String)
defVersusNext = label "defVersusNext" $ do
  let delimiter = try (many (satisfy (== ' ')) <* char ':') <|> "" <$ char ':'
      line      = some (satisfy (/= '\n')) <* newline
  indent <- gets fst
  sum <$> count indent (1 <$ satisfy (== ' '))
  hw    <- someTill (satisfy (\x -> x /= '\n' && x /= ':')) (lexeme delimiter)
  ln1   <- line
  mRest <- fmap fst <$> optional (try indentedTextBlockUntil')
  case mRest of
    Just mn -> return (hw, ln1 <> "\n" <> mn)
    Nothing -> return (hw, ln1)


-- | Like 'indentedTextBlock' but with optional block terminator, @mEnd@, which
-- should expect no leading whitespace and to consume the rest of the line
-- (newline included) but none of the proceeding line.
indentedTextBlockUntil' :: Parser (String, Int)
indentedTextBlockUntil' = do
  let terminalLine = defVersusSeparator <* newline
  indent    <- sum <$> many (1 <$ satisfy (== ' ')) <?> "leading whitespace"
  minIndent <- gets fst
  --dbg ( show indent <> " <= " <> show minIndent ) (pure ())
  when (indent < minIndent)
       (fail "indentedTextBlockUntil': insufficient indentation")
  let
    lines = do
      line <- Nothing <$ terminalLine <|> Just <$> some (satisfy (/= '\n'))
      case line of
        Just ln -> do
          emptyLines <- concat <$> some (try (indentedEmptyLine indent))
          rest       <-
            try (count indent (satisfy (== ' ')) *> fmap (emptyLines <>) lines)
              <|> pure ""
          return $ ln <> rest
        Nothing -> return ""
  --updateIndentation indent
  (, indent) <$> lines


-- | List of inline comparisions separated by "--- vs ---", properly indented
-- and on a line of its own.
--
-- TODO extend defversus to allow n-way comparisons. As regards this parser,
-- all that need be done is replace @count 2@ with 'some'.
--
-- TODO (HERE FIXME) !!!! rewrite to expect one standard inline definition (no special
-- indentation for first line, some for remainder) and then assert that the
-- separator and further inlines should be indented appropriately.
-- (I say this because `curr`, a.t.m., parses 'defVersus' correctly only when
-- it has a trailing newline and it fails to strip the second headword (potter)
-- of itl leading whitespace).
defVersus :: Parser DefQuery
defVersus = do
  xs <- count 2 (inlineMeaning True)
  case xs of
    ((hw, mn) : [(hw', mn')]) -> return $ DefVersus hw mn hw' mn'
    _ -> fail "defVersus: `count 2` succeeded but did not parse two elements"



defVersusSeparator :: Parser String
defVersusSeparator = lexeme (string "--- vs ---")


----------
-- PAGE --
----------

-- Is there a difference between 'PEnd' and 'PFinish'?
pageNum :: Parser Entry
pageNum = do
  pg <-
    try (Page <$ symbol "p")
    <|> try (PStart <$ symbol "s")
    <|> try (PEnd <$ symbol "e")
    <|> (PFinish <$ symbol "f") -- FIXME redundant page tag.
  num <- lexeme L.decimal
  many (try emptyLine)
  return $ PN $ pg num


------------
-- QUOTE --
------------

-- Parses a quotation entry body (its timestamp should have been already
-- parsed) at a given indentation level. A quotation entry has the form:
--
--  > (q|quotation) [<page-number>]
--  >
--  > "<quotation-content>"
--  >
--  > [<attribution>]
--
--  Meant to store quotable excerpts for retrieval.
--
--  E.g.,
--  >  quotation
--  >
--  >  "There was no treachery too base for the world to commit. She knew
--  >  that..."
--  >
--  >  Mrs. Ramsey in "To the Lighthouse", by Virginia Woolf
--
-- Notes:
-- * The entry variant prefix may be @q | quotation@.
-- * The attribution is optional; inferred attributions are preferred in
--   searches.
-- * The entry variant prefix parser is expected not to have leading
--   whitespace.
quotation :: Parser Entry
quotation = label "quotation" $ do
  quotePrefix
  pg <- optional (lexeme L.decimal)
  many (try emptyLine)
  (body, indentLevel) <- quoteContent
  many (try emptyLine)
  -- consume correct amount of leading whitespace to enforce (continuation) of
  -- correct indentation.
  attr <-
    optional
    $  try
    $  count indentLevel (satisfy isTabOrSpace)
    *> someTill anySingle newline
    <* newline
  return $ Quotation body (fromMaybe "" attr) pg

-- | Parses quoted content. Assumes that whitespace preceding the opening
-- double quote has been preserved.
--
-- FIXME proper tab handling. Debar them or process them correctly.
quoteContent :: Parser (String, Int)
quoteContent = do
  let isDelimiter x = x == '\n' || x == '"'
      lines indent = do
        lineContent  <- many (satisfy (not . isDelimiter)) <?> "line content"
        terminalChar <- satisfy isDelimiter
        case terminalChar of
          '\n' -> do
            emptyLines <- concat <$> many (try (indentedEmptyLine indent))
            count indent (satisfy isTabOrSpace)
            ((emptyLines <> lineContent <> "\n") <>) <$> lines indent
          '"' -> return lineContent
          _   -> error "expected newline or double quote"
  -- N.B. assumes spaces
  indentLevel <-
    sum <$> manyTill (1 <$ satisfy (== ' ')) (char '"') <?> "leading whitespace"
  (, indentLevel) <$> lines indentLevel

quotePrefix :: Parser String
quotePrefix = L.lexeme spaceWithoutNewline qp <?> "quote prefix"
  where qp = try (string "quotation") <|> string "q"


----------
-- READ --
----------

-- | Parse (top-level) entry containing attribution for book or other similarly
-- cited written media.
read :: Parser Entry
read = label "read" $ do
  let quote = char '"'
  currentIndentLevel <- gets fst
  unless
    (currentIndentLevel == 0)
    (  fail
    $  "read: found read entry with non-zero indentation"
    <> show currentIndentLevel
    )
  try (symbol "read")
    <|> try (symbol "finish reading")
    <|> try (symbol "begin to read")
    <|> try (symbol "finish")
    <|> symbol "begin"
  title <- lexeme $ between quote quote (some (satisfy (/= '"')))
  optional $ symbol ","
  symbol "by"
  author <- someTill (satisfy (/= '\n')) newline
  return $ Read title author


-- Assumes no leading whitespace.
timestamp :: Parser TimeStamp
timestamp = label "timestamp" $ do
  h <- twoDigitNatural
  char ':'
  m <- twoDigitNatural
  char ':'
  s <- twoDigitNatural
  space1
  symbol "λ."
  return $ TimeStamp h m s

-- | Parses a two digit decimal integer.
twoDigitNatural :: (MonadParsec e s m, Token s ~ Char) => m Int
twoDigitNatural = do
  tens <- digitToInt <$> digitChar
  ones <- digitToInt <$> digitChar
  return $ tens * 10 + ones

-------------
-- HELPERS --
-------------

type Parser' = Parsec Void String
type Parser = ParsecT Void String (State (Int, Maybe TimeStamp))


type ParserM e s m = (MonadParsec e s m, Token s ~ Char)

symbol = L.symbol spaceWithoutNewline

lexeme = L.lexeme spaceWithoutNewline

sc :: ParserM e s m => m ()
sc = L.space space1 empty empty

spaceWithoutNewline :: (MonadParsec e s m, Token s ~ Char) => m ()
spaceWithoutNewline = L.space (void whitespaceNotNewline) empty empty

--whitespaceNotNewline :: Parser String
whitespaceNotNewline :: (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
whitespaceNotNewline = takeWhile1P (Just "tab or space") isTabOrSpace

-- Parse either a newline or a string of tabs and/or spaces followed by
-- newline.
emptyLine :: Parser String
emptyLine =
  (try (return <$> newline) <|> ((++ "\n") <$> whitespaceNotNewline <* newline))
    <?> "empty line"

-- | Like 'emptyLine' but drops the first @indentLevel@ spaces from each
-- line that isn't just a newline.
indentedEmptyLine :: Int -> Parser String
indentedEmptyLine indentLevel = trim indentLevel <$> emptyLine
 where
  trim _ [] = []
  trim 0 xs = xs
  trim n l@(x : xs) | x == ' '  = trim (n - 1) xs
                    | otherwise = l

isTabOrSpace :: Char -> Bool
isTabOrSpace ' '  = True
isTabOrSpace '\t' = True
isTabOrSpace _    = False

----------
-- MISC --
----------

-- indentation-sensitive parsing
indented :: Parser (String, [String]) -- prefix and body line
indented = L.nonIndented sc (L.indentBlock sc p)
 where
  p = do
    prefix <- quotePrefix
    return (L.IndentMany Nothing (return . (prefix, )) (try qItem))

--qItem :: Parser String
qItem = L.lexeme spaceWithoutNewline p
  where p = some (satisfy (\x -> x /= '\n' && x /= '\f' && x /= '\r'))


pt p = flip runState (0, Nothing) . runParserT p ""

pt' p input =
  let parse s = runState (runParserT p "" s) (0, Nothing)
      (pState, customState) = parse input
  in  do
        case pState of
          Left  e -> putStr (errorBundlePretty e)
          Right x -> pPrint x
        pPrint customState
