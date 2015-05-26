{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

data Fetch = FetchURL { url :: Text, sha256 :: Text }
           | NoFetch
           deriving Generic

fetchOptions :: Options
fetchOptions = defaultOptions
               { constructorTagModifier = map Char.toLower
               , sumEncoding = ObjectWithSingleField
               }

instance FromJSON Fetch where
  parseJSON = genericParseJSON fetchOptions

instance ToJSON Fetch where
  toJSON = genericToJSON fetchOptions

data Build = MelpaPackage { recipe :: Text }
           | ElpaPackage
           deriving Generic

buildOptions :: Options
buildOptions = defaultOptions
               { constructorTagModifier = map Char.toLower
               , sumEncoding = ObjectWithSingleField
               }

instance FromJSON Build where
  parseJSON = genericParseJSON buildOptions

instance ToJSON Build where
  toJSON = genericToJSON buildOptions

data Package = Package
               { ver :: Text
               , deps :: [Text]
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
