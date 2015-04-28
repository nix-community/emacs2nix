{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Git where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Version

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
