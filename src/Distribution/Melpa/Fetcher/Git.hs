{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Git
       ( module Distribution.Melpa.Fetcher.Git.Types
       , hash
       ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Git.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe -> IO (Maybe Package)
hash melpa nixpkgs stable name arch rcp = runMaybeT $ do
  let Git _git@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name _git
  _git <- return _git { commit = Just _commit }
  _hash <- prefetch nixpkgs name _git
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = fromMaybe HM.empty (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Git _git }
    , Package.hash = _hash
    }

getCommit :: FilePath -> Bool -> Text -> Git -> MaybeT IO Text
getCommit melpa stable name Fetcher {..} = MaybeT $ do
  let env = HM.fromList
            $ [ ("melpa", T.pack melpa), ("name", name), ("url", url) ]
            ++ maybeToList ((,) "commit" <$> commit)
            ++ maybeToList ((,) "branch" <$> branch)
  HM.lookup name <$> runScript script env
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> Git -> MaybeT IO Text
prefetch nixpkgs name Fetcher {..} = MaybeT $ do
  let env = HM.fromList
            $ [ ("nixpkgs", T.pack nixpkgs), ("name", name), ("url", url) ]
            ++ maybeToList ((,) "commit" <$> commit)
            ++ maybeToList ((,) "branch" <$> branch)
  HM.lookup name <$> runScript "prefetch.sh" env
