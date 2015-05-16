{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.GitHub ( GitHub, fetchGitHub ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

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
fetchGitHub = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let GitHub _github@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (githubEnv name _github)
  _github <- return _github { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (githubEnv name _github)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = GitHub _github }
    , Package.hash = _hash
    }

githubEnv :: Text -> GitHub -> HashMap Text Text
githubEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "github"), ("name", name), ("repo", repo) ]
  ++ maybeToList ((,) "commit" <$> commit)
  ++ maybeToList ((,) "branch" <$> branch)
-}
