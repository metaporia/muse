{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Store.Sqlite.Types where

import           Data.Aeson
import           Database.Persist.TH
import           GHC.Generics

data DefTag
  = Headwords'
  | Inline'
  | Comparison'
  deriving (Eq, Show, Read, Generic)

instance ToJSON DefTag where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefTag

derivePersistField "DefTag"

data PageTag
  = Page'
  | PStart'
  | PEnd'
  | PFinish'
  deriving (Eq, Show, Read, Generic)

instance ToJSON PageTag where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PageTag

derivePersistField "PageTag"

-- | A list of tags, possesed by all variants but 'Null' & 'PageNum'.
newtype Tags = Tags [String] deriving (Eq, Show, Generic)

instance ToJSON Tags where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Tags
