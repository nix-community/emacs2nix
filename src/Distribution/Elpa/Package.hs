{-# LANGUAGE DeriveGeneric #-}

module Distribution.Elpa.Package where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types (defaultOptions, genericParseJSON, genericToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics

data Package =
  Package
  { ver :: [Integer]
  , deps :: Maybe (Map Text [Integer])
  , desc :: Text
  , dist :: Text -- TODO: replace with an enumeration
  , sha256 :: Maybe Text
  , archive :: Maybe String
  , broken :: Maybe Bool
  }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
