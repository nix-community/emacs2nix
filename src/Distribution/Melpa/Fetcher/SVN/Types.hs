{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Fetcher.SVN.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

data SVN =
  Fetcher
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON SVN where
  toJSON = genericToJSON defaultOptions

instance FromJSON SVN where
  parseJSON = genericParseJSON defaultOptions
