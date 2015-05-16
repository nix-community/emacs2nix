{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.CVS
       ( module Distribution.Melpa.Fetcher.CVS.Types
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
import Distribution.Melpa.Fetcher.CVS.Types
import Distribution.Melpa.Package (Package(Package))
import qualified Distribution.Melpa.Package as P
import Distribution.Melpa.Recipe
import Distribution.Melpa.Utils

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ True name _ _ = left (name <> ": stable fetcher 'cvs' not implemented")
hash _ nixpkgs False name arch rcp = do
  let CVS _cvs@(Fetcher {..}) = fetcher rcp
  _hash <- prefetch nixpkgs name Nothing (cvsEnv name _cvs)
  return Package
    { P.ver = A.ver arch
    , P.deps = maybe [] HM.keys (A.deps arch)
    , P.recipe = rcp { fetcher = CVS _cvs }
    , P.hash = _hash
    }

cvsEnv :: Text -> CVS -> HashMap Text Text
cvsEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "cvs"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "branch" <$> branch)
