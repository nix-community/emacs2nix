{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

updateMelpa :: FilePath -> FilePath -> Bool -> IO (Map Text Package)
updateMelpa melpa nixpkgs stable = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory melpa
    make stable [ "clean-json" ]
    make stable [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
  putStrLn (show (M.size archive) ++ " packages in")
  mpkgs <- for (M.toList archive) $ \(name, arch) -> do
    pkg <- runMaybeT $ do
      rcp <- MaybeT $ return $ M.lookup name recipes
      MaybeT $ hash melpa nixpkgs stable name arch rcp
    case pkg of
      Just _ -> return ()
      Nothing -> T.putStrLn (name <> ": no package")
    return $ (,) name <$> pkg
  let pkgs = M.fromList (catMaybes mpkgs)
  putStrLn (show (M.size pkgs) ++ " packages out")
  return pkgs
