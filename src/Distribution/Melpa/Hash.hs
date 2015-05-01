{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Hash where

import Data.Text (Text)

import Control.Error
import Distribution.Melpa.Archive
import Distribution.Melpa.Fetcher
import qualified Distribution.Melpa.Fetcher.Bzr as Bzr
import qualified Distribution.Melpa.Fetcher.CVS as CVS
import qualified Distribution.Melpa.Fetcher.Darcs as Darcs
import qualified Distribution.Melpa.Fetcher.Fossil as Fossil
import qualified Distribution.Melpa.Fetcher.Git as Git
import qualified Distribution.Melpa.Fetcher.GitHub as GitHub
import qualified Distribution.Melpa.Fetcher.Hg as Hg
import qualified Distribution.Melpa.Fetcher.SVN as SVN
import qualified Distribution.Melpa.Fetcher.Wiki as Wiki
import Distribution.Melpa.Package
import Distribution.Melpa.Recipe

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arc rcp =
  (case fetcher rcp of
    Git _ -> Git.hash
    GitHub _ -> GitHub.hash
    Bzr _ -> Bzr.hash
    Hg _ -> Hg.hash
    Darcs _ -> Darcs.hash
    Fossil _ -> Fossil.hash
    SVN _ -> SVN.hash
    CVS _ -> CVS.hash
    Wiki _ -> Wiki.hash
   ) melpa nixpkgs stable name arc rcp
