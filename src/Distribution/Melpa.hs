{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2016 Thomas Tuegel

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa ( updateMelpa ) where

import Control.Concurrent ( getNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types
import Data.Foldable ( toList )
import Data.HashMap.Strict ( HashMap )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import System.Directory ( createDirectoryIfMissing, copyFile )
import System.FilePath
import qualified System.IO.Streams.Attoparsec as S

import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Git as Git
import Distribution.Melpa.Fetcher
import Distribution.Melpa.Melpa
import Distribution.Melpa.PkgInfo
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Hash as Nix
import Distribution.Nix.Index
import qualified Distribution.Nix.Name as Nix
import qualified Distribution.Nix.Package.Melpa as Recipe ( Recipe(..) )
import qualified Distribution.Nix.Package.Melpa as Nix
import Exceptions
import Process
import Paths_emacs2nix ( getDataFileName )

updateMelpa :: FilePath
            -> Stable
            -> FilePath  -- ^ temporary workspace
            -> FilePath  -- ^ dump MELPA recipes here
            -> FilePath  -- ^ map of Emacs names to Nix names
            -> Bool  -- ^ only generate the index
            -> Set Emacs.Name
            -> IO ()
updateMelpa melpaDir stable workDir melpaOut namesMapFile indexOnly packages = do
  namesMap <- Nix.readNames namesMapFile

  melpaCommit <- Git.revision melpaDir Nothing []
  let melpa = Melpa {..}

  recipes <- readRecipes melpa

  let
    selected = Map.filterWithKey (\k _ -> isSelected k) recipes
      where
        isSelected name = Set.null packages || Set.member name packages

  createDirectoryIfMissing True workDir

  sem <- newQSem =<< getNumCapabilities

  let
    update name fetcher =
      Concurrently
      $ bracket (waitQSem sem) (\_ -> signalQSem sem)
      $ \_ -> getPackage melpa stable workDir namesMap (name, fetcher)
    updateKeys (_, pkg) = (Nix.pname pkg, Nix.expression pkg)
  updates <-
    runConcurrently
    $ if indexOnly
      then pure Map.empty
      else Map.fromList . map updateKeys . Map.toList
           <$> Map.traverseMaybeWithKey update selected

  existing <- readIndex melpaOut

  recipeNames <- Set.fromList <$> traverse (Nix.getName namesMap) (Map.keys recipes)
  selectedNames <- Set.fromList <$> traverse (Nix.getName namesMap) (Map.keys selected)

  let
    -- | Was the recipe removed?
    removed = \name -> not (Set.member name recipeNames) && isSelected name
      where
        isSelected name =
          Set.null selectedNames || Set.member name selectedNames
    updated
      | indexOnly = Map.union updates existing
      | otherwise =
          Map.union updates
          $ Map.filterWithKey (\k _ -> (not . removed) k) existing

  writeIndex melpaOut updated

data PackageException = PackageException Text SomeException
  deriving (Show, Typeable)

instance Exception PackageException


{-|

To create an expression for an Emacs package, we need several pieces of
information:

1. name
2. version
3. source
  a. the method to fetch the source
  b. the commit (or other token) to fetch the source reproducibly
  c. the hash of the source
4. dependencies
5. exact recipe

The name is available from the enumerated list of recipes or the arguments
on the command line. The version and source can be obtained after the package
is unpacked by 'package-build'. The dependencies are most easily determined
after the package is built by 'package-build'. It is not strictly necessary to
build every package just to write the Nix expression, but the build status can
be used to mark packages broken as appropriate. The exact recipe is available
at any time; we track this so that the Nix expression will reproduce the exact
build even if the recipe is changed upstream.

As elucidated above, there are two phases of the 'package-build' workflow which
we must execute: 'package-build-checkout' (to checkout the package) and
'package-build-package' (to build the checked-out source). 'package-build'
expects to be run from the MELPA tree, so in order to build each package in
isolation (and to keep the MELPA tree clean) we construct enough of a "fake"
tree that 'package-build' is happy. For our purposes, it suffices to create a
directory with one recipe (the target package) in the "recipes"
subdirectory. 'package-build' will create a "working" subdirectory with the
package source and a "packages" subdirectory with the build product.

 -}

getPackage :: Melpa -> Stable -> FilePath
           -> HashMap Emacs.Name Nix.Name
           -> (Emacs.Name, Fetcher)
           -> IO (Maybe Nix.Package)
getPackage melpa@(Melpa {..}) stable tmpDir namesMap (Emacs.fromName -> name, fetcher) =
  showExceptions $ mapExceptionIO (PackageException name) $ do
    let
      recipeFile = recipeFileName melpa name

      packageDir = tmpDir </> T.unpack name
      recipesDir = packageDir </> "recipes"
      workingDir = packageDir </> "working"
      archiveDir = packageDir </> "packages"
      sourceDir = workingDir </> T.unpack name

    createDirectoryIfMissing True recipesDir
    createDirectoryIfMissing True workingDir
    createDirectoryIfMissing True archiveDir
    copyFile recipeFile (recipesDir </> T.unpack name)

    melpaRecipe <- freezeRecipe melpa name
    pkgInfo <- build melpa stable name packageDir
    (_, fetch) <- Nix.prefetch name =<< freeze fetcher melpa sourceDir

    nixName <- Nix.getName namesMap (Emacs.Name name)
    nixDeps <- mapM (Nix.getName namesMap . Emacs.Name) (toList $ deps pkgInfo)

    pure
      Nix.Package
      { Nix.pname = nixName
      , Nix.version = version pkgInfo
      , Nix.fetch = fetch
      , Nix.deps = nixDeps
      , Nix.recipe = melpaRecipe
      }

build :: Melpa -> Stable -> Text -> FilePath -> IO PkgInfo
build melpa Stable {..} name packageDir =
  do
    buildEl <- getDataFileName "build.el"
    let
      args =
        [ "-Q", "--batch"
        , "-L", packageBuildDir melpa
        , "-l", buildEl
        , "-f", if stable then "build-stable" else "build"
        , T.unpack name
        ]
      cwd = Just packageDir
    runInteractiveProcess "emacs" args cwd Nothing $ \out -> do
      r <- liftIO (parseEither parsePkgInfo <$> S.parseFromStream json' out)
      case r of
        Left err -> throwIO (ParsePkgInfoError err)
        Right pkgInfo -> pure pkgInfo

recipeFileName :: Melpa -> Text -> FilePath
recipeFileName Melpa {..} (T.unpack -> name) = melpaDir </> "recipes" </> name

freezeRecipe :: Melpa -> Text -> IO Nix.Recipe
freezeRecipe melpa@(Melpa {..}) name = do
  let recipe = recipeFileName melpa name
  hash <- Nix.hash recipe
  commit <- Git.revision melpaDir Nothing [recipe]
  pure
    Nix.Recipe
    { Recipe.ename = name
    , Recipe.commit = commit
    , Recipe.sha256 = hash
    }

data NoVersion = NoVersion
  deriving (Show, Typeable)

instance Exception NoVersion

data ParsePkgInfoError = ParsePkgInfoError String
  deriving (Show, Typeable)

instance Exception ParsePkgInfoError
