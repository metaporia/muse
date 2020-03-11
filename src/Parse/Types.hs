{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Parse.Types where

import           Control.Lens.TH                ( makePrisms )
import           Control.Monad                  ( void )

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , genericToEncoding
                                                , defaultOptions
                                                , toEncoding
                                                )
import           GHC.Generics                   ( Generic )
import           Store.Sqlite.Types             ( Tags )


type Author = String
type Attr = String
type Body = String
type Headword = String
type Meaning = String
type Title = String
type Quote = String


data LogEntry
  = Dump String
  | TabTsEntry (Int, TimeStamp, Entry, Tags)
  deriving (Eq, Show, Generic)

getTimeStamp :: LogEntry -> Maybe TimeStamp
getTimeStamp (TabTsEntry (_, ts, _, _)) = Just ts
getTimeStamp _                       = Nothing

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON LogEntry

data DefQuery
  = Defn (Maybe PgNum)
         [Headword]
  | InlineDef Headword
              Meaning
  | DefVersus Headword
              Meaning
              Headword
              Meaning
  deriving (Eq, Generic, Show)

instance ToJSON DefQuery where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefQuery

type PgNum = Integer

data PageNum
  = Page PgNum
  | PStart PgNum
  | PEnd PgNum
  | PFinish PgNum
  deriving (Eq, Show, Generic)

instance ToJSON PageNum where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PageNum



isInlineDef :: DefQuery -> Bool
isInlineDef (InlineDef _ _) = True
isInlineDef _               = False

isDefVersus :: DefQuery -> Bool
isDefVersus DefVersus{} = True
isDefVersus _           = False

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

-- | Represents log timestamp (likely) parsed from the following format: "hh:mm:ss λ."
data TimeStamp = TimeStamp
  { hr :: Int
  , min :: Int
  , sec :: Int
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON TimeStamp where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimeStamp

-- | Relative duration, conversion from which expects rollover, not clipping,
-- as this is meant as a container for user-entered years, months, and days.
-- Thus, `RelDur 1000 1000 1000` ought to be a valid input to whichever
-- conversion function is used.
data RelDur = RelDur
  { yy :: Integer
  , mm :: Integer
  , dd :: Integer
  } deriving (Eq, Show)



makePrisms ''Entry

makePrisms ''LogEntry
