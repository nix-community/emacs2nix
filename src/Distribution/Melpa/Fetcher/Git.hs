{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher.Git where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data Git =
  Fetcher
  { url :: Text
  , commit :: Maybe Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Git where
  toJSON = genericToJSON defaultOptions

instance FromJSON Git where
  parseJSON = genericParseJSON defaultOptions
