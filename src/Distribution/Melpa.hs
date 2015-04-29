{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Exception (bracket)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)

import Distribution.Melpa.Archive (readArchive)
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe (readRecipes)

updateMelpa :: FilePath -> IO (HashMap Text Package)
updateMelpa melpa = _

make :: Bool -> [Text] -> IO ()
make stable args_ = callProcess "make" (map T.unpack args)
  where args = "SHELL=/bin/sh" : (if stable then ("STABLE=t" :) else id) args_

updateMelpaStable :: FilePath -> IO (HashMap Text Package)
updateMelpaStable melpa = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory melpa
    make True [ "clean-packages", "clean-json" ]
    make True [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
  _
