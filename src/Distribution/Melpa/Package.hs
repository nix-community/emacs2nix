{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Archive
import Distribution.Melpa.Recipe
import Distribution.Melpa.Version

data Package =
  Package
  { ver :: Version
  , deps :: [Text]
  , recipe :: Recipe
  , hash :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions
