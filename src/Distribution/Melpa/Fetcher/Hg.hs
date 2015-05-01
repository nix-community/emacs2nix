{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Hg
       ( module Distribution.Melpa.Fetcher.Hg.Types
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
import Distribution.Melpa.Fetcher.Hg.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> IO (Maybe Package)
hash melpa nixpkgs stable name arch rcp = runMaybeT $ do
  let Hg _hg@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name _hg
  _hg <- return _hg { commit = Just _commit }
  _hash <- prefetch nixpkgs name _hg
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] M.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Hg _hg }
    , Package.hash = _hash
    }

getCommit :: FilePath -> Bool -> Text -> Hg -> MaybeT IO Text
getCommit melpa stable name hg = MaybeT $ do
  let env = HM.singleton "melpa" (T.pack melpa) <> hgEnv name hg
  HM.lookup name <$> runScript script env
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> Hg -> MaybeT IO Text
prefetch nixpkgs name hg = MaybeT $ do
  let env = HM.singleton "nixpkgs" (T.pack nixpkgs) <> hgEnv name hg
  HM.lookup name <$> runScript "prefetch.sh" env

hgEnv :: Text -> Hg -> HashMap Text Text
hgEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "hg"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
