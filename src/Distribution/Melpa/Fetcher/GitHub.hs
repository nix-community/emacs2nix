{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.GitHub
       ( module Distribution.Melpa.Fetcher.GitHub.Types
       , hash
       ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.GitHub.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> IO (Maybe Package)
hash melpa nixpkgs stable name arch rcp = runMaybeT $ do
  let GitHub _github@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name _github
  _github <- return _github { commit = Just _commit }
  _hash <- prefetch nixpkgs name _github
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] M.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = GitHub _github }
    , Package.hash = _hash
    }

getCommit :: FilePath -> Bool -> Text -> GitHub -> MaybeT IO Text
getCommit melpa stable name github@(Fetcher {..}) = MaybeT $ do
  let env = HM.singleton "melpa" (T.pack melpa) <> githubEnv name github
  HM.lookup name <$> runScript script env
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> GitHub -> MaybeT IO Text
prefetch nixpkgs name github@(Fetcher {..}) = MaybeT $ do
  let env = HM.singleton "nixpkgs" (T.pack nixpkgs) <> githubEnv name github
  HM.lookup name <$> runScript "prefetch.sh" env

githubEnv :: Text -> GitHub -> HashMap Text Text
githubEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "github"), ("name", name), ("repo", repo) ]
  ++ maybeToList ((,) "commit" <$> commit)
  ++ maybeToList ((,) "branch" <$> branch)
