{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Recipe
import Distribution.Melpa.Version

data Package =
  Package
  { ver :: Version
  , deps :: [Text]
  , recipe :: Recipe
  , hash :: Text
  }
  deriving Generic

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
