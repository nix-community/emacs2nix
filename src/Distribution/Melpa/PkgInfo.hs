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

module Distribution.Melpa.PkgInfo
  ( PkgInfo (..)
  , readArchives
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types ( (.:) )
import qualified Data.Aeson.Types as Aeson
import Data.Foldable ( toList )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific ( floatingOrInteger )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import Data.Version ( Version, makeVersion )
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Attoparsec as Stream
import Data.Text.Prettyprint.Doc ( (<+>) )
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Distribution.Emacs.Name as Emacs
import Distribution.Melpa.Melpa
import Exceptions


data PkgInfo =
  PkgInfo
  { version :: !Version
  , deps :: !(Set Text)
  , commit :: !Text
  }


parseVersion :: Aeson.Value -> Aeson.Parser Version
parseVersion =
  Aeson.withArray "version" $ fmap makeVersion . traverse parseNatural . toList
  where
    parseNatural =
      Aeson.withScientific "natural number" $ \x ->
        case floatingOrInteger x of
          Left (r :: Double) -> fail ("not an integer: " ++ show r)
          Right i -> pure i


parseDeps :: Aeson.Value -> Aeson.Parser (Set Text)
parseDeps =
  \case
    Aeson.Null -> pure Set.empty
    Aeson.Object o -> pure (Set.fromList $ toList $ HashMap.keys o)
    _ -> fail "expected map of dependencies or null"


parseCommit :: Aeson.Value -> Aeson.Parser Text
parseCommit = Aeson.withText "commit" return


parsePkgInfo :: Aeson.Value -> Aeson.Parser PkgInfo
parsePkgInfo =
  Aeson.withObject "pkg-info" $ \obj ->
    do
      version <- parseVersion =<< obj .: "ver"
      deps <- parseDeps =<< obj .: "deps"
      props <- obj .: "props"
      commit <- parseCommit =<< props .: "commit"
      pure PkgInfo { version, deps, commit }


parseArchives :: Aeson.Value -> Aeson.Parser (HashMap Emacs.Name PkgInfo)
parseArchives =
  Aeson.withObject "archives"
    (traverse parsePkgInfo . mapKeys Emacs.Name)
  where
    mapKeys f = HashMap.fromList . map mapKey1 . HashMap.toList
      where
        mapKey1 (k, v) = (f k, v)


{- | Read archives from MELPA.

Read archives from MELPA into a map from package name to the 'PkgInfo' for each
package. The map keys are the /Emacs/ package names (not yet sanitized for
Nix). A @HashMap@ is used because random access is required to check if recipes
have been deleted.

Before this function is called, the archive must be compiled using the
MELPA @Makefile@.

See also: 'archiveJson'

-}
readArchives :: Melpa -> IO (HashMap Emacs.Name PkgInfo)
readArchives melpa =
  let archiveFile = archiveJson melpa in
  inContext ("reading" <+> Pretty.pretty archiveFile)
  $ Stream.withFileAsInput archiveFile $ \archiveInput ->
    do
      archives <- Stream.parseFromStream Aeson.json' archiveInput
      case Aeson.parseEither parseArchives archives of
        Left err -> throwM (ParseFilesError err)
        Right result -> return result
