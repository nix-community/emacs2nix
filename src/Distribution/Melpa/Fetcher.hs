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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Fetcher ( Fetcher (..), readRecipes ) where

import Data.Aeson.Types ( (.:), (.:?) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as Text
import System.FilePath
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Attoparsec as Stream

import qualified Distribution.Bzr as Bzr
import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Git as Git
import qualified Distribution.Hg as Hg
import Distribution.Melpa.Melpa
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.SVN as SVN
import qualified Distribution.Wiki as Wiki
import Paths_emacs2nix ( getDataFileName )
import Exceptions
import Process


-- | A @Fetcher@ is parsed from a MELPA recipe and can be frozen to (ultimately)
-- produce a Nix expression which will fetch the exact source of the package.
-- @Fetcher@ can be parsed from JSON with 'parseFetcher'. Calling @freeze@ with
-- the local path to the package source produces a 'Nix.Fetch' which is used
-- to retrieve an exact version of the package source.
newtype Fetcher = Fetcher { freeze :: Melpa -> FilePath -> IO Nix.Fetch }


-- | Read recipes from MELPA into a map from package name to the 'Fetcher' for
-- each package. The map keys are the /Emacs/ package names (not yet
-- sanitized for Nix). A @HashMap@ is used because random access is required
-- to check if recipes have been deleted.
readRecipes :: Melpa -> IO (HashMap Emacs.Name Fetcher)
readRecipes melpa = do
  let recipesDir = melpaDir melpa </> "recipes"
  dumpRecipesEl <- getDataFileName "scripts/dump-recipes.el"
  let args = [ "-Q"
             , "--batch"
             , "-L", packageBuildDir melpa
             , "-l", dumpRecipesEl
             , "-f", "dump-recipes-json", recipesDir
             ]
  runInteractiveProcess "emacs" args Nothing Nothing $ \out ->
    do
      value <- Stream.parseFromStream Aeson.json' out
      case Aeson.parseEither parseFetchers value of
        Left err ->
          do
            let msg = "error reading recipes: " <> Text.pack err
            Stream.write (Just msg) =<< Stream.encodeUtf8 Stream.stderr
            return HashMap.empty
        Right recipes -> return recipes


-- | Parse a map of package names to MELPA recipes from the JSON encoding of
-- MELPA recipes.
parseFetchers :: Aeson.Value -> Aeson.Parser (HashMap Emacs.Name Fetcher)
parseFetchers = Aeson.withObject "recipes" parseFetcher1
  where
    mapKeys f = HashMap.fromList . map mapKey1 . HashMap.toList
      where
        mapKey1 (k, v) = (f k, v)
    parseFetcher1 =
      HashMap.traverseWithKey parseFetcher . mapKeys Emacs.Name


-- | Parse a 'Fetcher' from the JSON encoding of a MELPA recipe.
parseFetcher :: Emacs.Name -> Aeson.Value -> Aeson.Parser Fetcher
parseFetcher (Emacs.fromName -> name) =
  Aeson.withObject "recipe" $ \rcp ->
    do
      fetcher <- rcp .: "fetcher"
      case fetcher :: Text of
        "bitbucket" ->
          do
            repo <- rcp .: "repo"
            let url = "https://bitbucket.com/" <> repo
            pure Fetcher
              { freeze = \_ src -> Nix.fetchHg url <$> Hg.revision src }
        "bzr" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \_ src -> Nix.fetchBzr url <$> Bzr.revision src }
        "git" ->
          do
            url <- rcp .: "url"
            branch <- rcp .:? "branch"
            pure Fetcher
              { freeze = \melpa src ->
                  do
                    files <- getFiles melpa name
                    Nix.fetchGit url branch <$> Git.revision src branch files
              }
        "github" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher
              { freeze = \melpa src ->
                  do
                    files <- getFiles melpa name
                    Nix.fetchGitHub owner repo <$> Git.revision src Nothing files
              }
        "gitlab" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher
              { freeze = \melpa src ->
                  do
                    files <- getFiles melpa name
                    Nix.fetchGitLab owner repo <$> Git.revision src Nothing files
              }
        "cvs" ->
          do
            cvsRoot <- rcp .: "url"
            cvsModule <- rcp .: "module"
            pure Fetcher { freeze = \_ _ -> pure $ Nix.fetchCVS cvsRoot cvsModule }
        "hg" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \_ src -> Nix.fetchHg url <$> Hg.revision src }
        "svn" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \_ src -> Nix.fetchSVN url <$> SVN.revision src }
        "wiki" ->
          do
            url <- rcp .:? "url"
            pure Fetcher
              { freeze = \_ _ ->
                  do
                    rev <- Wiki.revision name url
                    pure $ Nix.fetchURL rev (Just $ name <> ".el")
              }
        _ -> fail ("fetcher `" ++ Text.unpack fetcher ++ "' not implemented")


getFiles :: Melpa -> Text -> IO [FilePath]
getFiles melpa name =
  do
    buildEl <- getDataFileName "scripts/build.el"
    let
      args =
        [ "-Q", "--batch"
        , "-L", packageBuildDir melpa
        , "-l", buildEl
        , "-f", "files"
        , Text.unpack name
        ]
      cwd = Just (melpaDir melpa)
    runInteractiveProcess "emacs" args cwd Nothing $ \out -> do
      r <- Aeson.parseEither Aeson.parseJSON <$> Stream.parseFromStream Aeson.json' out
      case r of
        Left err -> throwM (ParseFilesError err)
        Right pkgInfo -> pure pkgInfo
