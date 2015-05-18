{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Fossil ( Fossil, fetchFossil ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Fossil =
  Fossil
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Fossil where
  toJSON = wrapFetcher "fossil" . genericToJSON defaultOptions

instance FromJSON Fossil where
  parseJSON = genericParseJSON defaultOptions

fetchFossil :: Fetcher Fossil
fetchFossil = Fetcher {..}
  where
    getRev name _ _ = left (name <> ": no fetcher for 'fossil'")
    prefetch name _ _ = left (name <> ": no fetcher for 'fossil'")
