{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.Fossil.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data Fossil =
  Fetcher
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Fossil where
  toJSON = genericToJSON defaultOptions

instance FromJSON Fossil where
  parseJSON = genericParseJSON defaultOptions
