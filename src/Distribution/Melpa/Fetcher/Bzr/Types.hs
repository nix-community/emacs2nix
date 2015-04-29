{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.Bzr.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data Bzr =
  Fetcher
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Bzr where
  toJSON = genericToJSON defaultOptions

instance FromJSON Bzr where
  parseJSON = genericParseJSON defaultOptions
