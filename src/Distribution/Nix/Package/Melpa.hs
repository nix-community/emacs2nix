{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package.Melpa where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Aeson.Types ( defaultOptions, genericParseJSON, genericToJSON )
import Data.Text (Text)
import GHC.Generics

data Melpa
  = Melpa { commit :: !Text
          , sha256 :: !Text
          }
  deriving Generic

instance FromJSON Melpa where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Melpa where
  toJSON = genericToJSON defaultOptions
