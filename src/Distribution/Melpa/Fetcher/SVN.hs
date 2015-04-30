{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.SVN
       ( module Distribution.Melpa.Fetcher.SVN.Types
       , hash
       ) where

import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.SVN.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> IO (Maybe Package)
hash melpa nixpkgs stable name arch rcp = runMaybeT $ do
  let SVN _svn@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name _svn
  _svn <- return _svn { commit = Just _commit }
  _hash <- prefetch nixpkgs name _svn
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = fromMaybe HM.empty (Archive.deps arch)
    , Package.recipe = rcp { fetcher = SVN _svn }
    , Package.hash = _hash
    }

getCommit :: FilePath -> Bool -> Text -> SVN -> MaybeT IO Text
getCommit melpa stable name svn = MaybeT $ do
  let env = HM.singleton "melpa" (T.pack melpa) <> svnEnv name svn
  HM.lookup name <$> runScript script env
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> SVN -> MaybeT IO Text
prefetch nixpkgs name svn = MaybeT $ do
  let env = HM.singleton "nixpkgs" (T.pack nixpkgs) <> svnEnv name svn
  HM.lookup name <$> runScript "prefetch.sh" env

svnEnv :: Text -> SVN -> HashMap Text Text
svnEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "svn"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
