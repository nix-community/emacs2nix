{-# LANGUAGE DeriveGeneric #-}

module Distribution.Melpa.Package where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Recipe
import Distribution.Melpa.Version

data Package =
  Package
  { ver :: Version
  , deps :: HashMap Text Version
  , recipe :: Recipe
  , hash :: Text
  }
  deriving (Eq, Generic)

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
