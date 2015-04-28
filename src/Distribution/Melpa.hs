{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

import Distribution.Melpa.Archive (readArchive)
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe (readRecipes)

updateMelpa :: FilePath -> IO (HashMap Text Package)
updateMelpa melpa = _

make :: Bool -> [Text] -> IO ()
make stable args_ = callProcess "make" args
  where args = "SHELL=/bin/sh" : (if stable then ("STABLE=t" :) else id) args_

updateMelpaStable :: FilePath -> IO (HashMap Text Package)
updateMelpaStable melpa = do
  withd melpa $ do
    make True [ "clean-packages", "clean-json" ]
    make True [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
