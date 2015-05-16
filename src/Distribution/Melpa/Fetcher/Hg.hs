{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Hg
       ( module Distribution.Melpa.Fetcher.Hg.Types
       , hash
       ) where

import Control.Error
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Hg.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let Hg _hg@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (hgEnv name _hg)
  _hg <- return _hg { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (hgEnv name _hg)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Hg _hg }
    , Package.hash = _hash
    }

hgEnv :: Text -> Hg -> HashMap Text Text
hgEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "hg"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
