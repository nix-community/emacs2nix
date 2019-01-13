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

module Distribution.Melpa.Melpa
  ( Melpa (..)
  , ParseMelpaError (..)
  , packageBuildDir
  , htmlDir
  , packagesDir
  , recipesDir
  , packageExpressionNix
  , archiveJson
  , recipesJson
  , recipeFile
  , workingDir
  , packageSource
  ) where

import Control.Exception ( Exception )
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Typeable ( Typeable )
import Data.Version ( Version, showVersion )
import System.FilePath

import qualified Distribution.Emacs.Name as Emacs


data Melpa =
  Melpa
  { melpaDir :: FilePath
  , melpaCommit :: Text
  }


data ParseMelpaError = ParseMelpaError String
  deriving (Show, Typeable)

instance Exception ParseMelpaError


packageBuildDir :: Melpa -> FilePath
packageBuildDir Melpa {..} = melpaDir </> "package-build"


htmlDir :: Melpa -> FilePath
htmlDir Melpa { melpaDir } = melpaDir </> "html"


packagesDir :: Melpa -> FilePath
packagesDir Melpa { melpaDir } = melpaDir </> "packages"


recipesDir :: Melpa -> FilePath
recipesDir Melpa { melpaDir } = melpaDir </> "recipes"


archiveJson :: Melpa -> FilePath
archiveJson melpa = htmlDir melpa </> "archive" <.> "json"


recipesJson :: Melpa -> FilePath
recipesJson melpa = htmlDir melpa </> "recipes" <.> "json"


packageExpressionNix :: Melpa -> Emacs.Name -> Version -> FilePath
packageExpressionNix melpa ename version =
    packagesDir melpa </> filename
  where
    filename =
        concat
            ( [ Text.unpack (Emacs.fromName ename)
              , "-"
              , showVersion version
              , ".nix"
              ] :: [String]
            )


recipeFile :: Melpa -> Emacs.Name -> FilePath
recipeFile melpa ename =
    recipesDir melpa </> (Text.unpack . Emacs.fromName) ename


workingDir :: Melpa -> FilePath
workingDir Melpa { melpaDir } = melpaDir </> "working"


packageSource :: Melpa -> Emacs.Name -> FilePath
packageSource melpa ename =
    workingDir melpa </> (Text.unpack . Emacs.fromName) ename
