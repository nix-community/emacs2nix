{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

data Fetch = FetchURL { url :: Text, sha256 :: Text }
           deriving Generic

fetchOptions :: Options
fetchOptions = defaultOptions
               { constructorTagModifier = map Char.toLower
               , sumEncoding = defaultTaggedObject { tagFieldName = "fetcher" }
               }

instance FromJSON Fetch where
  parseJSON = genericParseJSON fetchOptions

instance ToJSON Fetch where
  toJSON = genericToJSON fetchOptions

data Package = Package
               { ver :: Text
               , deps :: [Text]
               , fetch :: Fetch
               }
             deriving Generic

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
