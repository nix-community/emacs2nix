{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Error hiding (err)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import System.FilePath
import qualified System.IO.Streams as S

import Distribution.Melpa.Archive (readArchive)
import Distribution.Melpa.Hash
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe (readRecipes)

make :: FilePath -> Bool -> [Text] -> IO ()
make melpa stable args_ = do
  (inp, out, err, pid) <- S.runInteractiveProcess "make" args (Just melpa) Nothing
  S.write Nothing inp
  S.supply out S.stdout
  S.supply err S.stderr
  _ <- S.waitForProcess pid
  return ()
  where args = "SHELL=/bin/sh" : (if stable then ("STABLE=t" :) else id) (map T.unpack args_)

updateMelpa :: FilePath -> FilePath -> Bool -> IO (HashMap Text Package)
updateMelpa melpa nixpkgs stable = do
  putStrLn "building archive.json and recipes.json..."
  make melpa stable [ "clean-json" ]
  make melpa stable [ "json" ]
  archive <- readArchive (melpa </> "html/archive.json")
  recipes <- readRecipes (melpa </> "html/recipes.json")
  let countKnown = HM.size archive
  putStrLn (show countKnown ++ " known packages")
  let discardMissing = HM.fromList . catMaybes
      getPackage (name, arch) = do
        result <- runEitherT $ do
          rcp <- (HM.lookup name recipes) ?? (name <> ": no recipe")
          hash melpa nixpkgs stable name arch rcp
        case result of
          Right pkg -> return (Just (name, pkg))
          Left err_ -> T.putStrLn err_ >> return Nothing
  pkgs <- discardMissing <$> traverse getPackage (HM.toList archive)
  let countGen = HM.size pkgs
  putStrLn (show countGen ++ " generated packages")
  return pkgs
