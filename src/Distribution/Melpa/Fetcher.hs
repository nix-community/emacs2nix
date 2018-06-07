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
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Fetcher ( Fetcher (..), readRecipes ) where

import Control.Exception
import Data.Aeson.Types ( (.:), (.:?) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy ( Text )
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Data.Text.Read as Text
import qualified Network.Http.Client as HTTP
import System.FilePath
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Attoparsec as Stream
import Text.Taggy.Parser ( taggyWith )
import Text.Taggy.Types ( Tag (..), Attribute(..) )

import qualified Distribution.Bzr as Bzr
import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Git as Git
import qualified Distribution.Hg as Hg
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.SVN as SVN
import Paths_emacs2nix ( getDataFileName )


-- | A @Fetcher@ is parsed from a MELPA recipe and can be frozen to (ultimately)
-- produce a Nix expression which will fetch the exact source of the package.
-- @Fetcher@ can be parsed from JSON with 'parseFetcher'. Calling @freeze@ with
-- the local path to the package source produces a 'Nix.Fetch' which is used
-- to retrieve an exact version of the package source.
newtype Fetcher = Fetcher { freeze :: FilePath -> IO Nix.Fetch }


readRecipes :: FilePath -> IO (Map Emacs.Name Fetcher)
readRecipes melpaDir = do
  let packageBuildDir = melpaDir </> "package-build"
      packageBuildEl = "package-build.el"
      recipesDir = melpaDir </> "recipes"
  dumpRecipesEl <- getDataFileName "dump-recipes.el"
  let args = [ "-Q"
             , "--batch"
             , "-L", packageBuildDir
             , "-l", packageBuildEl
             , "-l", dumpRecipesEl
             , "-f", "dump-recipes-json", recipesDir
             ]
  bracket
    (Stream.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> Stream.waitForProcess pid)
    (\(_, sout, serr, _) -> do
         result <-
           catch
           (Aeson.parseEither parseFetchers <$> Stream.parseFromStream Aeson.json' sout)
           (\(SomeException exn) ->
              do
                Stream.supply serr Stream.stderr
                pure (Left $ show exn))
         case result of
           Left err -> do
             let msg = "error reading recipes: " <> Text.pack err
             Stream.write (Just msg) =<< Stream.encodeUtf8 Stream.stderr
             return Map.empty
           Right recipes -> return recipes)


-- | Parse a map of package names to MELPA recipes from the JSON encoding of
-- MELPA recipes.
parseFetchers :: Aeson.Value -> Aeson.Parser (Map Emacs.Name Fetcher)
parseFetchers =
  Aeson.withObject "recipes"
  $ Map.traverseWithKey parseFetcher
  . Map.mapKeys Emacs.Name
  . Map.fromList
  . HashMap.toList


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
              { freeze = \src -> Nix.fetchHg url <$> Hg.revision src }
        "bzr" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \src -> Nix.fetchBzr url <$> Bzr.revision src }
        "git" ->
          do
            url <- rcp .: "url"
            branch <- rcp .:? "branch"
            pure Fetcher
              { freeze = \src ->
                  Nix.fetchGit url branch <$> Git.revision src branch []
              }
        "github" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher
              { freeze = \src ->
                  Nix.fetchGitHub owner repo <$> Git.revision src Nothing []
              }
        "gitlab" ->
          do
            (owner, Text.drop 1 -> repo) <- Text.breakOn "/" <$> rcp .: "repo"
            pure Fetcher
              { freeze = \src ->
                  Nix.fetchGitLab owner repo <$> Git.revision src Nothing []
              }
        "cvs" ->
          do
            cvsRoot <- rcp .: "url"
            cvsModule <- rcp .: "module"
            pure Fetcher { freeze = \_ -> pure $ Nix.fetchCVS cvsRoot cvsModule }
        "hg" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \src -> Nix.fetchHg url <$> Hg.revision src }
        "svn" ->
          do
            url <- rcp .: "url"
            pure Fetcher
              { freeze = \src -> Nix.fetchSVN url <$> SVN.revision src }
        "wiki" ->
          do
            url <- rcp .:? "url"
            pure Fetcher
              { freeze = \_ ->
                  Nix.fetchURL <$> freezeWikiURL name url <*> pure (Just $ name <> ".el")
              }
        _ -> fail ("fetcher `" ++ Text.unpack fetcher ++ "' not implemented")

defaultWikiURL :: Text -> Maybe Integer -> Text
defaultWikiURL name rev =
  let
    query = (\r -> "?revision=" <> Text.pack (show r)) <$> rev
  in
    "https://www.emacswiki.org/emacs/download/" <> name <> ".el" <> fromMaybe "" query


freezeWikiURL :: Text -> Maybe Text -> IO Text
freezeWikiURL _ (Just url) = pure url
freezeWikiURL name Nothing = defaultWikiURL name <$> guessWikiRevision name

guessWikiRevision :: Text -> IO (Maybe Integer)
guessWikiRevision name =
  do
    body <- getAsText revisionsPageUrl
    return $ findLatestRevision $ taggyWith True body
    where
        revisionsPageUrl :: ByteString
        revisionsPageUrl = Text.encodeUtf8 $ "https://www.emacswiki.org/emacs?action=history;id=" <> name <> ".el"

        defaultRevisionAttr = Attribute "href" (defaultWikiURL name Nothing)

        getAsText :: ByteString -> IO Lazy.Text
        getAsText url = Text.Lazy.decodeUtf8 . ByteString.Lazy.fromStrict <$> HTTP.get url HTTP.concatHandler

        readDecimal :: Text -> Maybe Integer
        readDecimal aText =
          case Text.decimal aText of
            Left _ -> Nothing
            Right (i, _) -> Just i

        revisionPrefix = "Revision "

        findLatestRevision [] = Nothing
        findLatestRevision (TagOpen "a" attrs _ : TagText aText : tags)
          | elem defaultRevisionAttr attrs && Text.isPrefixOf revisionPrefix aText =
              readDecimal $ Text.drop (Text.length revisionPrefix) aText
          | otherwise = findLatestRevision tags
        findLatestRevision (_:tags) = findLatestRevision tags
