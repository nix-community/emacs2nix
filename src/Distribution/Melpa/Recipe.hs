{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Text (Text)
import Network.Http.Client (get)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Files (Files)
import qualified Distribution.Melpa.Files as Files

data Recipe =
  Recipe
  { fetcher :: Fetcher
  , files :: Maybe Files
  }
  deriving (Eq, Read, Show)

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \obj -> do
    fetch <- obj .: "fetcher"
    let val = Object obj
    files <- traverse Files.fromMelpa (HM.lookup "files" obj)
    fetcher <- case fetch of
      "git" -> Git <$> parseJSON val
      "github" -> GitHub <$> parseJSON val
      "bzr" -> Bzr <$> parseJSON val
      "hg" -> Hg <$> parseJSON val
      "darcs" -> Darcs <$> parseJSON val
      "fossil" -> Fossil <$> parseJSON val
      "svn" -> SVN <$> parseJSON val
      "cvs" -> CVS <$> parseJSON val
      "wiki" -> Wiki <$> parseJSON val
      _ -> fail ("unknown fetcher: " ++ fetch)
    return Recipe {..}

instance ToJSON Recipe where
  toJSON Recipe {..} =
      addFiles $ case fetcher of
        Git git -> addFetcher "git" (toJSON git)
        GitHub github -> addFetcher "github" (toJSON github)
        Bzr bzr -> addFetcher "bzr" (toJSON bzr)
        Hg hg -> addFetcher "hg" (toJSON hg)
        Darcs darcs -> addFetcher "darcs" (toJSON darcs)
        Fossil fossil -> addFetcher "fossil" (toJSON fossil)
        SVN svn -> addFetcher "svn" (toJSON svn)
        CVS cvs -> addFetcher "cvs" (toJSON cvs)
        Wiki wiki -> addFetcher "wiki" (toJSON wiki)
    where
      addFetcher (Object obj) fetch = Object (HM.insert "fetcher" fetch obj)
      addFetcher other _ = other
      addFiles (Object obj) = Object (HM.insert "files" (toJSON files) obj)
      addFiles other = other

fetchRecipes :: IO (HashMap Text Recipe)
fetchRecipes =
  get "http://melpa.org/recipes.json" $ \_ inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result

readRecipes :: FilePath -> IO (HashMap Text Recipe)
readRecipes path =
  S.withFileAsInput path $ \inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result
