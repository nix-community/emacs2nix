{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Git
       ( module Distribution.Melpa.Fetcher.Git.Types
       , hash
       ) where

import Control.Error hiding (runScript)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as Archive
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Git.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as Package
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let Git _git@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (gitEnv name _git)
  _git <- return _git { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (gitEnv name _git)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Git _git }
    , Package.hash = _hash
    }

gitEnv :: Text -> Git -> HashMap Text Text
gitEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "git"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
  ++ maybeToList ((,) "branch" <$> branch)
