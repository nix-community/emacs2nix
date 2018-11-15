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
  , Stable (..)
  , packageBuildDir
  , htmlDir
  , archiveJson
  , recipesJson
  ) where

import Control.Exception ( Exception )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import System.FilePath


data Melpa =
  Melpa
  { melpaDir :: FilePath
  , melpaCommit :: Text
  }


data ParseMelpaError = ParseMelpaError String
  deriving (Show, Typeable)

instance Exception ParseMelpaError


newtype Stable = Stable { stable :: Bool }


packageBuildDir :: Melpa -> FilePath
packageBuildDir Melpa {..} = melpaDir </> "package-build"


htmlDir :: Melpa -> FilePath
htmlDir Melpa { melpaDir } = melpaDir </> "html"


archiveJson :: Melpa -> FilePath
archiveJson melpa = htmlDir melpa </> "archive" <.> "json"


recipesJson :: Melpa -> FilePath
recipesJson melpa = htmlDir melpa </> "recipes" <.> "json"
