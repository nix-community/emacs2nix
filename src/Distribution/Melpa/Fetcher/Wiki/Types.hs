{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Wiki.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Process (readProcess)

import Distribution.Melpa.Version

data Wiki =
  Fetcher
  { url :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Wiki where
  toJSON = genericToJSON defaultOptions

instance FromJSON Wiki where
  parseJSON = genericParseJSON defaultOptions
