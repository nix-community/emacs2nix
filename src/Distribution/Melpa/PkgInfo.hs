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

module Distribution.Melpa.PkgInfo ( PkgInfo (..), parsePkgInfo ) where

import Data.Aeson.Types
import Data.Foldable ( toList )
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific ( floatingOrInteger )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import Data.Version ( Version, makeVersion )


data PkgInfo =
  PkgInfo
  { version :: !Version
  , deps :: !(Set Text)
  }


parseVersion :: Value -> Parser Version
parseVersion =
  withArray "version" $ fmap makeVersion . traverse parseNatural . toList
  where
    parseNatural =
      withScientific "natural number" $ \x ->
      case floatingOrInteger x of
        Left (r :: Double) -> fail ("not an integer: " ++ show r)
        Right i -> pure i


parsePkgInfo :: Value -> Parser PkgInfo
parsePkgInfo =
  withObject "pkg-info" $ \obj ->
  do
    version <- parseVersion =<< obj .: "ver"
    deps <- parseDeps =<< obj .: "deps"
    pure PkgInfo {..}
  where
    parseDeps =
      \case
        Null -> pure Set.empty
        Object o -> pure (Set.fromList $ toList $ HashMap.keys o)
        _ -> fail "expected map of dependencies or null"
