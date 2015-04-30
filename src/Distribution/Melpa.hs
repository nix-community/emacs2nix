{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callProcess)

import Distribution.Melpa.Archive (readArchive)
import Distribution.Melpa.Hash
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe (readRecipes)

make :: Bool -> [Text] -> IO ()
make stable args_ = callProcess "make" (map T.unpack args)
  where args = "SHELL=/bin/sh" : (if stable then ("STABLE=t" :) else id) args_

updateMelpa :: FilePath -> FilePath -> Bool -> IO (HashMap Text Package)
updateMelpa melpa nixpkgs stable = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory melpa
    make stable [ "clean-packages", "clean-json" ]
    make stable [ "packages" ]
    make stable [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
  mpkgs <- for (HM.toList archive) $ \(name, arch) -> do
    pkg <- runMaybeT $ do
      rcp <- MaybeT $ return $ HM.lookup name recipes
      MaybeT $ hash melpa nixpkgs stable name arch rcp
    return $ (,) name <$> pkg
  let pkgs = HM.fromList (catMaybes mpkgs)
      count n | stable = show n ++ " stable packages"
              | otherwise = show n ++ " packages"
  putStrLn (count $ HM.size pkgs)
  return pkgs
