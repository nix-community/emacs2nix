{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.Darcs where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data Darcs =
  Fetcher
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Darcs where
  toJSON = genericToJSON defaultOptions

instance FromJSON Darcs where
  parseJSON = genericParseJSON defaultOptions
