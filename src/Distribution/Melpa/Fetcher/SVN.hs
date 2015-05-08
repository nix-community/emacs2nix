{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.SVN
       ( module Distribution.Melpa.Fetcher.SVN.Types
       , hash
       ) where

import Control.Error
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Prelude hiding (FilePath)
import Turtle

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.SVN.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let SVN _svn@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name (svnEnv name _svn)
  _svn <- return _svn { commit = Just _commit }
  _hash <- prefetch nixpkgs name (svnEnv name _svn)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = SVN _svn }
    , Package.hash = _hash
    }

svnEnv :: Text -> SVN -> HashMap Text Text
svnEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "svn"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
