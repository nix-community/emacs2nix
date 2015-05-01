{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Error hiding (err)
import Control.Exception (bracket)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
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
    make stable [ "clean-json" ]
    make stable [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
  let countIn = HM.size archive
  putStrLn (show countIn ++ " packages in")
  let discardMissing = HM.fromList . catMaybes
      getPackage (name, arch) = do
        result <- runEitherT $ do
          rcp <- (HM.lookup name recipes) ?? (name <> ": no recipe")
          hash melpa nixpkgs stable name arch rcp
        case result of
          Right pkg -> return (Just (name, pkg))
          Left err -> T.putStrLn err >> return Nothing
  pkgs <- discardMissing <$> traverse getPackage (HM.toList archive)
  let countOut = HM.size pkgs
  putStrLn (show countOut ++ " packages out")
  return pkgs
