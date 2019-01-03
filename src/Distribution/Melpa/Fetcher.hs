{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2018 Thomas Tuegel

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

module Distribution.Melpa.Fetcher ( Fetcher (..), readRecipes ) where

import Data.Aeson.Types ( (.:), (.:?) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Attoparsec as Stream

import qualified Distribution.Emacs.Name as Emacs
import Distribution.Melpa.Melpa
import qualified Distribution.Nix.Fetch as Nix


-- | A @Fetcher@ is parsed from a MELPA recipe and can be frozen to (ultimately)
-- produce a Nix expression which will fetch the exact source of the package.
-- @Fetcher@ can be parsed from JSON with 'parseFetcher'. Calling @freeze@ with
-- the commit ID produces a 'Nix.Fetch' which is used to retrieve an exact
-- version of the package source.
newtype Fetcher = Fetcher { freeze :: Text -> Nix.Fetch }


{- | Read recipes from MELPA.

Read recipes from MELPA into a map from package name to the 'Fetcher' for each
package. The map keys are the /Emacs/ package names (not yet sanitized for
Nix). A @HashMap@ is used because random access is required to check if recipes
have been deleted.

Before this function is called, the recipe archive must be compiled using the
MELPA @Makefile@.

See also: 'recipesJson'

-}
readRecipes :: Melpa -> IO (HashMap Emacs.Name Fetcher)
readRecipes melpa =
  Stream.withFileAsInput (recipesJson melpa) $ \recipesInput ->
    do
      recipes <- Stream.parseFromStream Aeson.json' recipesInput
      case Aeson.parseEither parseFetchers recipes of
        Left err ->
          do
            let msg = "error reading recipes: " <> Text.pack err
            Stream.write (Just msg) =<< Stream.encodeUtf8 Stream.stderr
            return HashMap.empty
        Right result -> return result


-- | Parse a map of package names to MELPA recipes from the JSON encoding of
-- MELPA recipes.
parseFetchers
  :: Aeson.Value  -- ^ Recipes
  -> Aeson.Parser (HashMap Emacs.Name Fetcher)
parseFetchers =
  Aeson.withObject "recipes"
    (traverse parseFetcher . mapKeys Emacs.Name)
  where
    mapKeys f = HashMap.fromList . map mapKey1 . HashMap.toList
      where
        mapKey1 (k, v) = (f k, v)


-- | Parse a 'Fetcher' from the JSON encoding of a MELPA recipe.
parseFetcher :: Aeson.Value -> Aeson.Parser Fetcher
parseFetcher =
  Aeson.withObject "recipe" $ \rcp ->
    do
      fetcher <- rcp .: "fetcher"
      let sha256 = Nothing -- uninitialize in all cases
      case fetcher :: Text of
        "bitbucket" ->
          do
            repo <- rcp .: "repo"
            let
              url = "https://bitbucket.com/" <> repo
            pure Fetcher { freeze = \rev -> Nix.fetchHg Nix.Hg {..} }
        "git" ->
          do
            url <- rcp .: "url"
            branchName <- rcp .:? "branch"
            pure Fetcher { freeze = \rev -> Nix.fetchGit Nix.Git {..} }
        "github" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher { freeze = \rev -> Nix.fetchGitHub Nix.GitHub {..} }
        "gitlab" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher { freeze = \rev -> Nix.fetchGitLab Nix.GitLab {..} }
        "hg" ->
          do
            url <- rcp .: "url"
            pure Fetcher { freeze = \rev -> Nix.fetchHg Nix.Hg {..} }
        _ -> fail ("fetcher `" ++ Text.unpack fetcher ++ "' not implemented")
