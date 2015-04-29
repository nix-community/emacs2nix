module Distribution.Melpa.Fetcher.Darcs
       ( module Distribution.Melpa.Fetcher.Darcs.Types
       , hash
       ) where

import Data.Text (Text)

import Distribution.Melpa.Archive
import Distribution.Melpa.Fetcher.Darcs.Types
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe

hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> IO (Maybe Package)
hash _ _ _ _ _ _ = return Nothing
