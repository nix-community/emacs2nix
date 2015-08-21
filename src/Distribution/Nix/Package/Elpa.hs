{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package.Elpa
       ( Package(..)
       ) where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Aeson.Types ( defaultOptions, genericParseJSON, genericToJSON )
import Data.Text ( Text )
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch )

data Package
  = Package
    { version :: !Text
    , fetch :: !Fetch
    , deps :: ![Text]
    }
  deriving Generic

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
