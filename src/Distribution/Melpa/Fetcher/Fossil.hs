{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher.Fossil
       ( module Distribution.Melpa.Fetcher.Fossil.Types
       , hash
       ) where

import Control.Error
import Data.Text (Text)
import Data.Monoid ((<>))

import Distribution.Melpa.Archive
import Distribution.Melpa.Fetcher.Fossil.Types
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ _ name _ _ = left (name <> ": fetcher 'fossil' not implemented")
