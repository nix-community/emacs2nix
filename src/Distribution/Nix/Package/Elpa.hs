{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package.Elpa
       ( Package(..)
       , cleanNames
       ) where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Aeson.Types ( defaultOptions, genericParseJSON, genericToJSON )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text ( Text )
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch )
import Distribution.Nix.Name ( cleanName )

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

cleanNames :: Map Text Package -> Map Text Package
cleanNames = Map.map (\p -> p { deps = map cleanName (deps p) })
             . Map.mapKeys cleanName
