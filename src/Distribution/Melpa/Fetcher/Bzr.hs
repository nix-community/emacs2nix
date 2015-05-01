{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Bzr
       ( module Distribution.Melpa.Fetcher.Bzr.Types
       , hash
       ) where

import Control.Error
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)

import Distribution.Melpa.Archive (Archive)
import qualified Distribution.Melpa.Archive as A
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Bzr.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as P
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ True name _ _ = left (name <> ": stable fetcher 'bzr' not implemented")
hash melpa nixpkgs stable name arch rcp = do
  let Bzr _bzr@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name (bzrEnv name _bzr)
  _bzr <- return _bzr { commit = Just _commit }
  _hash <- prefetch nixpkgs name (bzrEnv name _bzr)
  return Package
    { P.ver = A.ver arch
    , P.deps = maybe [] HM.keys (A.deps arch)
    , P.recipe = rcp { fetcher = Bzr _bzr }
    , P.hash = _hash
    }

bzrEnv :: Text -> Bzr -> HashMap Text Text
bzrEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "bzr"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
