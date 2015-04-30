{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Wiki
       ( module Distribution.Melpa.Fetcher.Wiki.Types
       , hash
       ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Wiki.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> IO (Maybe Package)
hash _ nixpkgs _ name arch rcp = runMaybeT $ do
  let Wiki _wiki@(Fetcher {..}) = fetcher rcp
  _hash <- prefetch nixpkgs name _wiki
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = fromMaybe HM.empty (Archive.deps arch)
    , Package.recipe = rcp
    , Package.hash = _hash
    }

prefetch :: FilePath -> Text -> Wiki -> MaybeT IO Text
prefetch nixpkgs name wiki = MaybeT $ do
  let env = HM.singleton "nixpkgs" (T.pack nixpkgs) <> wikiEnv name wiki
  HM.lookup name <$> runScript "prefetch.sh" env

wikiEnv :: Text -> Wiki -> HashMap Text Text
wikiEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "wiki"), ("name", name) ]
  ++ maybeToList ((,) "url" <$> url)
