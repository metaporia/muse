{-# LANGUAGE InstanceSigs, OverloadedStrings, GADTs, QuasiQuotes,
  ScopedTypeVariables, FlexibleInstances, DeriveGeneric,
  TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parse.Entry
-- Copyright   :  2018 Keane Yahn-Kraft
-- License     :  GPL-3 (see the file LICENSE)
-- Maintainer  :  kyahnkrafft@me.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides parsers for `LogEntry` and its constituent types.
--
-- TODO
--
-- □  (!!) store quote as @Quotation Quote (Maybe Attr) (Maybe PgNum)@
--
-- □  add 'Poem' 'Entry' variant
--
-- □  Support entry tags. The proposed tags would be included inside square
-- brackets, e.g., @d 34 [<tag0>, ..., <tagN>] headword : meaning@. A tag
-- sequence should /follow/ the page number, if it's present. Empty tag
-- sequences are permitted but discouraged. Spaces are optional but permissible
-- between entry variant prefix, page number, tag sequence and definition body.
-- Note that the tags themselvel /must/ be non-empty.
--
--    This will be done (this is a description of coarse granulariy) as follows:
--
--      1. (DONE) Update the entry parser of each variant we mean to support tags with
--      the 'tag' parser.
--
--      2. (DONE) Add a tag list (@Maybe [String]@ or @[String]@?) to each entry
--      variant.
--
--      3. (YOU ARE HERE) Complete migration sqlite (write the damnable search function
--      already) and then store tags in each variant's table.
--
--      4. Include CLI subcommand to collect, within a date range, naturally,
--      entries with a certain tag. How to serialize or expose this entries is
--      as yet undetermined.
--
-----------------------------------------------------------------------------
module Parse.Entry where

import           Control.Applicative
import           Control.Lens.TH                ( makePrisms )
import           Control.Monad                  ( void )
import           Control.Monad.Trans.State
import           Data.Aeson              hiding ( Null
                                                , (<?>)
                                                )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                , second
                                                )
import           Data.Bool                      ( bool )
import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhile
                                                , dropWhileEnd
                                                , intercalate
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust )
import           Data.Semigroup                 ( (<>) )
import           Data.Time
import           Debug.Trace                    ( trace )
import           GHC.Generics            hiding ( Infix
                                                , Prefix
                                                )
import           Helpers
import           Parse
import           Prelude                 hiding ( min
                                                , quot
                                                )
import           Text.Parser.LookAhead
import qualified Text.PrettyPrint.ANSI.Leijen  as Leijen
import           Text.RawString.QQ
import           Text.Show.Pretty               ( pPrint )
import           Text.Trifecta           hiding ( Rendering
                                                , Span
                                                , double
                                                )

-- | Parse an quotation entry (body, without timestamp) of the form:
--
--  > "quotation [<page-number>]
--  >
--  >  <quotation-content>
--  >
--  > <attribution>"
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
--  TODO: conditionally omit capture of attribution when it can be inferred
--  from indentation context.
quotation :: Parser Entry
quotation = do
  _  <- try (lpad $ symbol "quotation") <|> lpad (symbol "q")
  pg <- optional digits
  skipOptional emptyLines
  --q <- lpad $ between quote quote (some $ noneOf "\"")
  q            <- lpad $ quote'
  -- FIXME see ~/sputum/muse/19.03.01 for example of valid log the breaks
  -- auto-attribution logic.
  -- TODO discard post quote attribution when indent >= 1
  --titleAuthEtc <- untilPNoTs' $ try (void $ timestamp) <|> try (void $ symbol "...") <|> eof
  titleAuthEtc <- linesNoTs <* skipOptional eof
  return $ Quotation (unwords . fmap trim . lines $ q) (trim titleAuthEtc) pg

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"0rvtbf" -- ['\\', '"', ...]
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\r\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

quote' :: Parser String
quote' = do
  char '"'
  s <- many character
  char '"'
  return $ concat s



-- | Parse an commentary entry (body, without timestamp) of the form:
--
--  > "(commentary | synthesis)
--  >
--  >  <content>"
--
-- Stores synthesis of, or commentary on, some piece of text, or merely the
-- surrounding definitions, quotations, etc.
--
-- TODO: context awareness (see above todo)
-- TODO support page-numbers and tags
commentary :: Parser Entry
commentary = do
  _    <- try (symbol "commentary") <|> symbol "synthesis"
  body <- entryBody
  _    <- many newline
  return . Commentary . unlines . fmap trim . lines $ body

-- TODO support tags??
book :: Parser Entry
book = do
  _ <- whiteSpace
  _ <-
    try (symbol "read")
    <|> try (symbol "begin to read")
    <|> try (symbol "finish reading")
    <|> symbol "finish"
  title       <- between quot quot (some $ noneOf "\"")
  _           <- optional $ symbol "," --
  _           <- symbol "by" <?> "expected attribution"
  -- what is this "canonical" business?
  isCanonical <- option False $ symbol "canonical" *> return True
  author      <- some (noneOf "\n")
  _           <- entryBody
  _           <- many newline
  return $ Read title author

-- TODO handle "\n    \n" w/o parser fantods
def :: Parser Entry
def = do
  dq <- (try toDefVersus <|> try inlineMeaning <|> toDefn)
  return . Def . trimDefQuery $ dq

-- | Extracts page number as one of:
--
--  > "p <num>" -- page number
--  > "s <num>" -- pg at start of reading session
--  > "e <num>" -- pg at end of reading session
--  > "f <num>" -- pg at end of book; shorthand for completion of book
page :: Parser Entry
page = do
  _ <- skipOptional (many space)
  -- FIXME parse prefix in same pass as digits: it induces a redundancy
  -- but should avoid the inexhaustive case expression, which seems in poor
  -- taste.
  --
  p <-
    try (const Page <$> symbolic 'p')
    <|> try (const PStart <$> symbolic 's')
    <|> try (const PEnd <$> symbolic 'e')
    <|> const PFinish
    <$> symbolic 'f'
  pg <- digits <?> "page digits"
  _  <- entryBody
  _  <- many newline
  return $ PN $ p pg

-- TODO add test case for ~/sputum/muse/17.10.17, parses only beginning of
-- dump and discards rest of file
dump :: Parser LogEntry
dump = Dump <$> (many space *> el *> manyTill anyChar (try $ newline <* el))
  where el = string "..."

nullE :: Parser Entry
nullE =
  const Null <$> skipOptional (many space *> (newline <?> "newline here"))

data LogEntry
  = Dump String
  | TabTsEntry (Int, TimeStamp, Entry)
  deriving (Eq, Show, Generic)

getTimeStamp :: LogEntry -> Maybe TimeStamp
getTimeStamp (TabTsEntry (_, ts, _)) = Just ts
getTimeStamp _                       = Nothing

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON LogEntry

logEntry :: Parser LogEntry
logEntry = do
  _ <- skipOptional (try emptyLines)
  let null = do
        (indent, ts) <- timestamp'
        _            <- void (skipOptional spacesNotNewline *> newline) <|> eof
        return $ TabTsEntry (indent, ts, Null)
      entry' = do
        (indent, ts) <- timestamp' <?> "timestamp"
        e            <-
          try book
          <|> try quotation
          <|> try commentary
          <|> try dialogue
          <|> try def
          <|> try page
          <|> try phrase
        return $ TabTsEntry (indent, ts, e)
  e <- try dump <|> try null <|> entry'
  _ <- void (skipOptional emptyLines <?> "emptyLines") <|> eof
  return e

tmp = do
  x <- lpad nullE
  _ <- void (skipOptional emptyLines) <|> eof
  return x

oldLogEntries :: Parser [LogEntry]
oldLogEntries =
  const []
    <$> (   try (void $ many space)
        <|> try (void emptyLines)
        <|> try eof
        <?> "eat eof"
        )
    *>  some logEntry

logEntries = validatedLogEntries
-- | Like 'logEntry' but this parser will enforce that no two timestamps are
-- duplicate by requiring each to be greater than and not equal to its
-- predecessor.
--
-- If the timestamp of an entry is found to be less than or equal to that which
-- came before it, some helpful error message should be constructed with the
-- location of the issue, and, ideally, some directions--or better yet, an
-- interactive timestamp editing interface--as to how they might correct the
-- issue so that the day in question's 'LogEntry' may parse successfully.
logEntry' :: Maybe TimeStamp -> Parser (LogEntry, Maybe TimeStamp)
logEntry' Nothing = do
  -- No timestamp means its the first entry in the day or a dump.  If a dump
  -- occurs at the start of the file, the parser will proceed without a
  -- timestamp and treat the first non-dump entry is the start of the file
  -- (that is, w.r.t. the timestamp passing logic). Should a dump be situated
  -- in the middle of a series of timestamped entries, the dump will be parsed
  -- normally and the last timestamp parsed will be passed through for
  -- comparision with the very next timestamp to be encountered.
  le <- logEntry
  -- No lookahead required as it's the first timestamp in the day's log.  As
  -- such, this branch cannot fail due to duplicate or unordered timestamps.
  return $ case le of
    Dump       _          -> (le, Nothing)
    TabTsEntry (_, ts, _) -> (le, Just ts)
logEntry' (Just previousTimeStamp) = do
  le <- lookAhead logEntry
  -- 'lookAhead' places the error caret at the end of well-formed input, in
  -- this case on the character of the malformed timestamp or its preceding
  -- whitespace.
  case le of
    Dump _ -> return (le, Just previousTimeStamp)
    TabTsEntry (_, currentTimeStamp, _) ->
      if previousTimeStamp < currentTimeStamp
        then return (le, Just currentTimeStamp)
        else raiseErr $ Err
          ( return
          $ Leijen.vcat
          $ (Leijen.text "Unordered or duplicate timestamps:" :)
-- (Leijen.line :)
          $ fmap Leijen.text
          $ (wordSensitiveLineWrap
              55
              (  "Found current timestamp, t1 = "
              <> show currentTimeStamp
              <> ", and previous timestamp, t0  = "
              <> show previousTimeStamp
              <> " where t0 >= t1, but timestamps MUST be unique and ordered smallest to greatest."
              )
            )
          )
          mempty
          mempty
          mempty

-- | If the parser yields a non-'Nothing' 'TimeStamp', it's included in the
-- output. Otherwise, the first parameter is used.
--
-- This is a "stateful" parser meant for use with 'statefulValidatedMany'. The
-- only "stateful" behavior is the choice of timestamp to pass along.
logEntryPassNewestTimeStamp
  :: Maybe TimeStamp -> Parser (LogEntry, Maybe TimeStamp)
logEntryPassNewestTimeStamp mTs = do
  le <- logEntry
  let mTs' = getTimeStamp le
  case mTs' of
    Just ts -> return (le, Just ts)
    Nothing -> return (le, mTs')

-- | If properly applied, this ensures that the 'LogEntry' parser will squeal
-- with volume and precision whenever any timestamps are duplicated or
-- non-ascending.
logEntrySquawk :: Maybe TimeStamp -> Maybe TimeStamp -> Maybe String
logEntrySquawk previous current = case (>=) <$> previous <*> current of
  Just b -> if b
    then Just
      (  "duplicate or out of order timestamps: "
      <> "prev = "
      <> show previous
      <> ", curr = "
      <> show current
      )
    else Nothing
  Nothing -> Nothing

---- | Like 'logEntry' but this parser will enforce that no two timestamps are
---- duplicate by requiring each to be greater than and not equal to its
---- predecessor.
----
---- If the timestamp of an entry is found to be less than or equal to that which
---- came before it, some helpful error message should be constructed with the
---- location of the issue, and, ideally, some directions--or better yet, an
---- interactive timestamp editing interface--as to how they might correct the
---- issue so that the day in question's 'LogEntry' may parse successfully.
--logEntry''
--  :: Maybe TimeStamp -> Parser (LogEntry, Maybe TimeStamp)
--logEntry'' Nothing = do
--  -- No timestamp means its the first entry in the day or a dump.  If a dump
--  -- occurs at the start of the file, the parser will proceed without a
--  -- timestamp and treat the first non-dump entry is the start of the file
--  -- (that is, w.r.t. the timestamp passing logic). Should a dump be situated
--  -- in the middle of a series of timestamped entries, the dump will be parsed
--  -- normally and the last timestamp parsed will be passed through for
--  -- comparision with the very next timestamp to be encountered.
--  le <- logEntry
--  -- No lookahead required as it's the first timestamp in the day's log.  As
--  -- such, this branch cannot fail due to duplicate or unordered timestamps.
--  return $ case le of
--    Dump       _          -> (le, Nothing)
--    TabTsEntry (_, ts, _) -> (le, Just ts)
--logEntry'' (Just previousTimeStamp) = do
--  le <- lookAhead logEntry
--  -- 'lookAhead' places the error caret at the end of well-formed input, in
--  -- this case on the character of the malformed timestamp or its preceding
--  -- whitespace.
--  case le of
--    Dump _ -> return (le, Just previousTimeStamp, Nothing)
--    TabTsEntry (_, currentTimeStamp, _) ->
--      if previousTimeStamp < currentTimeStamp
--        then return (le, Just currentTimeStamp, Nothing)
--        else return
--          ( le
--          , Just previousTimeStamp
--          , Just
--            (unlines
--              (wordSensitiveLineWrap
--                55
--                (  "Found current timestamp, t1 = "
--                <> show currentTimeStamp
--                <> ", and previous timestamp, t0  = "
--                <> show previousTimeStamp
--                <> " where t0 >= t1, but timestamps MUST be unique and ordered smallest to greatest."
--                )
--              )
--            )
--          )

-- | Inserts linebreaks intelligently.
--
-- It places them less than the given number of characters into a line if it
-- the alternative is to include only part of a word.
wordSensitiveLineWrap :: Int -> String -> [String]
wordSensitiveLineWrap n s =
  let takeNCharsWorth _ []      = ([], [])
      takeNCharsWorth n (h : t) = if length h < n
        then bimap (h :) id $ takeNCharsWorth (n - length h) t
        else ([], h : t)
      go ws =
        let (ln, rest) = takeNCharsWorth 80 ws
        in  if not (null rest)
              then unwords ln : go rest
              else unwords ln : [unwords rest]
  in  go (words s)


-- | Parse with context/state.
--
-- For example, in homage to the useful but utterly contrived, I show below how
-- to parse a list of comma-separated digits and count the length of the list a
-- single pass.
--
-- Given:
--
-- @
--  parse :: Parser a -> String -> Trifecta.Result a
--
--  digitWithCounter :: Int -> Parser (Int, Int)
--  digitWithCounter numDigits = do
--    n <- read . return <$> digit
--    return (n, numDigits + 1)
-- @
--
-- We have:
--
-- >>> parse (statefulMany 0 digitWithCounter) ""
-- Success([], 0)
--
-- and
--
-- >>> parse (statefulMany 0 digitWithCounter) "357"
-- Success([3,5,7], 3)
--
statefulMany :: s -> (s -> Parser (a, s)) -> Parser ([a], s)
statefulMany initialState p = go initialState
 where
  go s = do
    mx <- try (Just <$> p s) <|> return Nothing
    case mx of
      Just (a, s') -> first (a :) <$> go s'
      Nothing      -> return ([], s)

-- | 'threadMany' /basically/ suffices for efficient input validation (and
-- if/when I prettify the error message by digging into the parsers delta,
-- span, line information, it will be fully comparable to a normal "Trifecta"
-- error message--which is what we want).
--
-- However, I would prefer to write a greedy combinator like 'many' but that
-- has two failure modes: the first parses until it fails and then the whole
-- parser fails; the second parses until it fails and then returns the
-- collected results.  This better suits the intended usage of 'threadMany',
-- which is fusing parsing and (simplistic) fold-based property checking into a
-- single parser-embedded pass.
--
-- tl;dr; If the left parser fails, so does the 'threadMany''; if the right one
-- should fail, the parser keeps going as the failure signals that the parser
-- should return what's been collected--that is, the failure is due solely to
-- the absence of parseable input, /not/ a property violation.
--
-- For example:
--
-- >>> parse (statefulValidatedMany 0 squawkIfNotAscending shouldSquawk) "1234"
-- Success ([1,2,3,4],4)
--
-- but
--
-- >>> parse (statefulValidatedMany 0 squawkIfNotAscending shouldSquawk) "1233"
-- Failure (ErrInfo { _errDoc = "...expected ascending numbers...", .. })
--
--
-- TODO assert that the parser must not fail before the end of the file, and
-- then a failure will surface. The incident that prompts this is the lack of
-- helpful reporting of the cause of failure when a the preceding zeros of a
-- timestamp are omitted, which is a breaking syntax error but causes the
-- parser to silently fail and skip the entry before returning a list of
-- entries whose source was compliant.
statefulValidatedMany
  :: s -- ^ Initial state.
  -- | A predicate that returns an error message explaining why the given
  -- component parser didn't fail, but 'statefulValidatedMany' did. The older
  -- state is passed before the newer. Failure is indicated by the presence of
  -- a error string.
  -> (s -> s -> Maybe String)
  --  The stateful parser whose failure signals result collection, /not/ an
  -- error message--for that use the previous parameter.
  -> (s -> Parser (a, s))
  -> Parser ([a], s)
statefulValidatedMany initialState predicate parser =
  let go s = do
        maybeTup <- try (Just <$> parser s) <|> return Nothing
        case maybeTup of
          Nothing      -> return ([], s) -- parse failed (not predicate), and so we collect results
          Just (a, s') -> case predicate s s' of
            Just err -> unexpected err -- TODO fix linewrap issue (it's too short for long messages)
            Nothing  -> first (a :) <$> go s'
  in  go initialState

-- Example predicate
squawkIfNotAscending :: Int -> Int -> Maybe String
squawkIfNotAscending prev curr =
  if curr > prev then Nothing else Just "squawk: expected ascending numbers"

-- example stateful parser
shouldSquawk :: Int -> Parser (Int, Int)
shouldSquawk _ = double . read . return <$> digit

-- | Parses a file of 'LogEntry's and fails if its timesmamps are not strictly
-- ascending (i.e., a lack of duplicates is ensured).
validatedLogEntries :: Parser [LogEntry]
validatedLogEntries = do
  try (void $ many space) <|> try (void emptyLines) <|> try eof <?> "eat eof"
  fst
    <$> statefulValidatedMany Nothing logEntrySquawk logEntryPassNewestTimeStamp

-- keep for pretty err msg example
ascendingDigits' :: Int -> Parser (Either String (Int, Int))
ascendingDigits' previous = do
  current <- read . return <$> digit
  skipOptional (many space *> char ',' <* many space)
  return $ if previous < current
    then Right (current, current)
    else
      let
        err = explain emptyRendering $ Err
          (  Just
          $  Leijen.vcat
          $  fmap Leijen.text
          $  ("Unorderd or duplicate timestamps:" :)
          $  wordSensitiveLineWrap 55
          $  "Found current timestamp, t1 = "
          <> show current
          <> ", previous timestapm, t2 = "
          <> show previous
          <> " where t0 >= t1, but timestamps MUST be unique and ordered smallest to greatest."
          )
          mempty
          mempty
          mempty
      in      -- trace (show err) $
          Left "expected ascending comma-separated digits"


logEntries' :: Parser [LogEntry]
logEntries' = do
  try (void $ many space) <|> try (void emptyLines) <|> try eof <?> "eat eof"
  some logEntry
 where
  go p = do
    mLe <- try (Just <$> p) <|> return Nothing
    case mLe of
      Just le -> (le :) <$> go p
      Nothing -> return []


--some' :: Maybe s -> (Maybe s -> Parser (a, Maybe s)) -> Parser [a]
--some' initialState parser = do (a, ms) <- parser initialState
--                               (a:) <$> many' ms parser
--

-- TODO add N.B. field to as many variants as possible (poss. by adding (N.B |
-- n.b. | nota bene) parser to `untilP` in entryBody
data Entry
  = Def DefQuery
  | Read Title
         Author
  | Quotation Quote
              Attr
              (Maybe PgNum)
  | Commentary Body
  | PN PageNum
  | Phr Phrase
  | Dialogue String
  | Null -- ^ represents entry of only a timestamp
  deriving (Eq, Generic, Show)
instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entry

isQuotation :: Entry -> Bool
isQuotation (Quotation _ _ _) = True
isQuotation _                 = False

-- TODO TAG REFACTOR (Phrase and Def unification)
data DefQueryVariant
  = Phrase'
  | Defn'
  | InlineDef'
  | DefVersus'
  deriving (Eq, Show)

allDefVariants :: [DefQueryVariant]
allDefVariants = [Phrase', Defn', InlineDef', DefVersus']

-- | Note that until the much needed refactor in which 'DefQuery' and 'Phrase'
-- are unified under a single type with a tag list (the tags refactor will
-- allow this), the below jank will treat defined phrases as inline
-- definititions.
--
-- Should 'DefVersus' count as inline definitions?
defHasType :: DefQueryVariant -> Either Phrase DefQuery -> Bool
defHasType InlineDef' (Left (Defined _ _)) = True
--defHasType InlineDef' (Right (DefVersus _ _ _ _)) = True
defHasType variant    dq                   = variant == defQueryVariant dq

defQueryVariant :: Either Phrase DefQuery -> DefQueryVariant
defQueryVariant (Right (Defn      _ _    )) = Defn'
defQueryVariant (Right (InlineDef _ _    )) = InlineDef'
defQueryVariant (Right (DefVersus _ _ _ _)) = DefVersus'
defQueryVariant (Left  _                  ) = Phrase'

isDefn :: DefQuery -> Bool
isDefn (Defn _ _) = True
isDefn _          = False

data Phrase
  = Plural [Headword]
  | Defined Headword
            Meaning
  deriving (Eq, Show, Generic)

instance ToJSON Phrase where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Phrase

phrase :: Parser Entry
phrase = do
  whiteSpace
  try (symbol "phrase") <|> symbol "phr"
  tagList <- optional tags -- TODO tag refactor
  p       <- try definedPhrase <|> pluralPhrase
  many newline
  return $ Phr p

pluralPhrase :: Parser Phrase
pluralPhrase = do
  hws <- sepBy (some $ noneOf ",\n") (symbol ",") <* entryBody
  return $ Plural hws

definedPhrase :: Parser Phrase
definedPhrase = do
  hw <- many (noneOf ":") <* symbol ": "
  Defined hw <$> entryBody

dialogue :: Parser Entry
dialogue = do
  symbol "dialogue"
  eb <-
    intercalate "\n\n"
    .   fmap (unlines . fmap trim . lines)
    .   splitOn "\n\n"
    <$> entryBody
  return $ Dialogue eb

makePrisms ''Entry

makePrisms ''LogEntry
