{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.Hg.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data Hg =
  Fetcher
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Hg where
  toJSON = genericToJSON defaultOptions

instance FromJSON Hg where
  parseJSON = genericParseJSON defaultOptions
