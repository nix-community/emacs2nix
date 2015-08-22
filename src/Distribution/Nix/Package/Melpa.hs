{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package.Melpa
       ( Package(..), Recipe(..)
       , cleanNames
       ) where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Aeson.Types ( defaultOptions, genericParseJSON, genericToJSON )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch )
import Distribution.Nix.Name ( cleanName )

data Package
  = Package
    { version :: !Text
    , fetch :: !Fetch
    , deps :: ![Text]
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

cleanNames :: Map Text Package -> Map Text Package
cleanNames = Map.map (\p -> p { deps = map cleanName (deps p) })
             . Map.mapKeys cleanName
