{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.GitHub
       ( module Distribution.Melpa.Fetcher.GitHub.Types
       , hash
       ) where

import Control.Error
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.GitHub.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let GitHub _github@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name (githubEnv name _github)
  _github <- return _github { commit = Just _commit }
  _hash <- prefetch nixpkgs name (githubEnv name _github)
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
