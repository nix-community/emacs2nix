{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

import Distribution.Nix.Fetch

data Build = MelpaPackage { recipe :: Text, deps :: [Text] }
           | ElpaPackage { deps :: [Text] }
           deriving Generic

buildOptions :: Options
buildOptions = defaultOptions
               { constructorTagModifier = buildTagModifier
               , sumEncoding = ObjectWithSingleField
               }
  where
    buildTagModifier [] = []
    buildTagModifier (x:xs) = Char.toLower x : xs

instance FromJSON Build where
  parseJSON = genericParseJSON buildOptions

instance ToJSON Build where
  toJSON = genericToJSON buildOptions

data Package = Package
               { version :: Text
               , fetch :: Fetch
               , build :: Build
               }
             deriving Generic

packageOptions :: Options
packageOptions = defaultOptions { omitNothingFields = True }

instance FromJSON Package where
  parseJSON = genericParseJSON packageOptions

instance ToJSON Package where
  toJSON = genericToJSON packageOptions
