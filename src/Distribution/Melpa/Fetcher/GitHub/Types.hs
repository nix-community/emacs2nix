{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.GitHub.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data GitHub =
  Fetcher
  { repo :: Text
  , commit :: Maybe Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON GitHub where
  toJSON = genericToJSON defaultOptions

instance FromJSON GitHub where
  parseJSON = genericParseJSON defaultOptions
