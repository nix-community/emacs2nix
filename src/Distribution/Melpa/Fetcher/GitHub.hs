{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.GitHub ( GitHub, fetchGitHub ) where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Git (getRev_Git)

data GitHub =
  GitHub
  { repo :: Text
  , commit :: Maybe Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON GitHub where
  toJSON = wrapFetcher "github" . genericToJSON defaultOptions

instance FromJSON GitHub where
  parseJSON = genericParseJSON defaultOptions

fetchGitHub :: Fetcher GitHub
fetchGitHub = Fetcher {..}
  where
    getRev name GitHub {..} tmp = handleAll $ getRev_Git name branch tmp
    prefetch name GitHub {..} rev =
      prefetchWith name "nix-prefetch-git" args
      where
        args = [ "--url", T.unpack url, "--rev", T.unpack rev ]
        url = "https://github.com/" <> repo <> ".git"
