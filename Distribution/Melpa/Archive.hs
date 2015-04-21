{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Archive where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Version

data Archive = Archive { deps :: Maybe (HashMap Text Version) }
  deriving (Eq, Generic)

instance FromJSON Archive where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Archive where
  toJSON = genericToJSON defaultOptions
