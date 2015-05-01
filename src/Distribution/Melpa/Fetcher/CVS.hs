{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher.CVS
       ( module Distribution.Melpa.Fetcher.CVS.Types
       , hash
       ) where

import Control.Error
import Data.Monoid ((<>))
import Data.Text (Text)

import Distribution.Melpa.Archive
import Distribution.Melpa.Fetcher.CVS.Types
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ _ name _ _ = left (name <> ": fetcher 'cvs' not implemented")
