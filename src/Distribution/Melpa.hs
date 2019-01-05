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

{-# LANGUAGE TemplateHaskell #-}

module Distribution.Melpa (updateMelpa) where

import Control.Concurrent ( getNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Concurrent.QSem
import Data.Foldable ( toList )
import Data.HashSet ( HashSet )
import Nix.Expr (NExpr)
import Text.PrettyPrint.ANSI.Leijen ( Pretty, (<+>) )

import qualified Control.Monad.Extra as Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Directory as Directory
import qualified System.IO.Streams as Streams
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Melpa
import Distribution.Melpa.PkgInfo
import Distribution.Nix.Index
import Exceptions

import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Git as Git
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Hash as Nix
import qualified Distribution.Nix.Package.Melpa as Recipe ( Recipe(..) )
import qualified Distribution.Nix.Package.Melpa as Nix


data MissingRecipeError = MissingRecipeError Emacs.Name
mkException 'PrettyException ''MissingRecipeError

instance Pretty MissingRecipeError where
  pretty (MissingRecipeError ename) =
    "No recipe found for Emacs package" <+> squotes (Pretty.pretty ename)
    where
      squotes doc = "‘" <> doc <> "’"


updateMelpa
  :: FilePath
  -> HashSet Emacs.Name
  -> IO ()
updateMelpa melpaDir selectedNames = do
  melpaCommit <- Git.revision melpaDir Nothing []
  let melpa = Melpa {..}

  archives <- readArchives melpa
  recipes <- readRecipes melpa

  let
    select
      | HashSet.null selectedNames = id
      | otherwise =
        Map.filterWithKey (\name _ -> HashSet.member name selectedNames)
    selectedPackages = select (toMap archives)
      where
        toMap = Map.fromList . HashMap.toList

  sem <- newQSem =<< getNumCapabilities

  let
    update ename pkgInfo =
      concurrently $ catchPretty $ inContext context $ do
        recipe <- getRecipe
        getPackageExpression melpa ename pkgInfo recipe
      where
        concurrently =
            Concurrently . withQSem . const
          where
            withQSem = bracket (waitQSem sem) (\_ -> signalQSem sem)
        context =
            "package" <+> Pretty.string sname
          where
            tname = Emacs.fromName ename
            sname = T.unpack tname
        getRecipe =
          case HashMap.lookup ename recipes of
            Nothing -> throwM (MissingRecipeError ename)
            Just recipe -> return recipe
  updated <- runConcurrently (Map.traverseMaybeWithKey update selectedPackages)

  output <- Streams.encodeUtf8 Streams.stdout
  writeIndex output updated


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

getPackage
  :: Melpa
  -> Emacs.Name
  -> PkgInfo
  -> Fetcher
  -> IO Nix.Package
getPackage melpa@(Melpa {..}) ename pkgInfo fetcher =
  do
    recipe <- freezeRecipe melpa ename
    (_, fetch) <- Nix.prefetch (freeze fetcher commit)

    pure Nix.Package
      { ename
      , version
      , fetch
      , deps = Emacs.Name <$> toList (deps pkgInfo)
      , recipe
      }
  where
    PkgInfo { commit, version } = pkgInfo


getPackageExpression
  :: Melpa
  -> Emacs.Name
  -> PkgInfo
  -> Fetcher
  -> IO NExpr
getPackageExpression melpa ename pkgInfo fetcher =
  do
    Monad.ifM (Directory.doesFileExist nix)
      (Nix.readPackageExpression nix)
      (do
          package <- getPackage melpa ename pkgInfo fetcher
          let expr = Nix.expression package
          Nix.writePackageExpression nix expr
          return expr
      )
  where
    PkgInfo { version } = pkgInfo
    nix = packageExpressionNix melpa ename version


freezeRecipe :: Melpa -> Emacs.Name -> IO Nix.Recipe
freezeRecipe melpa@Melpa { melpaDir } ename = do
  let recipe = recipeFile melpa ename
  hash <- Nix.hash melpaDir recipe
  let sha256 = Just hash
  rev <- Git.revision melpaDir Nothing [recipe]
  pure Nix.Recipe { ename, rev, sha256 }
