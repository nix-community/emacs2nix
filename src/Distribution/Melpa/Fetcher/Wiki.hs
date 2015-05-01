{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Wiki
       ( module Distribution.Melpa.Fetcher.Wiki.Types
       , hash
       ) where

import Control.Error hiding (runScript)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Wiki.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ nixpkgs _ name arch rcp = do
  let Wiki _wiki@(Fetcher {..}) = fetcher rcp
  _hash <- prefetch nixpkgs name (wikiEnv name _wiki)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp
    , Package.hash = _hash
    }

wikiEnv :: Text -> Wiki -> HashMap Text Text
wikiEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "wiki"), ("name", name) ]
  ++ maybeToList ((,) "url" <$> url)
