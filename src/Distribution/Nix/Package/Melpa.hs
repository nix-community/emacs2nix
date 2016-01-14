{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package.Melpa
       ( Package(..), Recipe(..) ) where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Aeson.Types ( defaultOptions, genericParseJSON, genericToJSON )
import Data.Text (Text)
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch )
import Distribution.Nix.Name ( Name )

data Package
  = Package
    { pname :: !Name
    , ename :: !Text
    , version :: !Text
    , fetch :: !Fetch
    , deps :: ![Name]
    , recipe :: !Recipe
    }
  deriving Generic

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions

data Recipe
  = Recipe { commit :: !Text
           , sha256 :: !Text
           }
  deriving Generic

instance FromJSON Recipe where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Recipe where
  toJSON = genericToJSON defaultOptions
