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
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Fetcher.Bzr (Bzr)
import Distribution.Melpa.Fetcher.CVS (CVS)
import Distribution.Melpa.Fetcher.Darcs (Darcs)
import Distribution.Melpa.Fetcher.Fossil (Fossil)
import Distribution.Melpa.Fetcher.Git (Git)
import Distribution.Melpa.Fetcher.GitHub (GitHub)
import Distribution.Melpa.Fetcher.Hg (Hg)
import Distribution.Melpa.Fetcher.SVN (SVN)
import Distribution.Melpa.Fetcher.Wiki (Wiki)
import Distribution.Melpa.Files

data Recipe
  = Git !Git !Files
  | GitHub !GitHub !Files
  | Bzr !Bzr !Files
  | Hg !Hg !Files
  | Darcs !Darcs !Files
  | Fossil !Fossil !Files
  | SVN !SVN !Files
  | CVS !CVS !Files
  | Wiki !Wiki !Files
  deriving (Eq, Read, Show)

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \obj -> do
    fetcher <- obj .: "fetcher"
    let val = Object obj
    files <- obj .:? "files" .!= mempty
    -- let files = mempty
    case fetcher of
      "git" -> Git <$> parseJSON val <*> pure files
      "github" -> GitHub <$> parseJSON val <*> pure files
      "bzr" -> Bzr <$> parseJSON val <*> pure files
      "hg" -> Hg <$> parseJSON val <*> pure files
      "darcs" -> Darcs <$> parseJSON val <*> pure files
      "fossil" -> Fossil <$> parseJSON val <*> pure files
      "svn" -> SVN <$> parseJSON val <*> pure files
      "cvs" -> CVS <$> parseJSON val <*> pure files
      "wiki" -> Wiki <$> parseJSON val <*> pure files
      _ -> fail ("unknown fetcher: " ++ fetcher)

instance ToJSON Recipe where
  toJSON rcp =
    case rcp of
      Git git files -> addFetcher "git" (addFiles files (toJSON git))
      GitHub github files -> addFetcher "github" (addFiles files (toJSON github))
      Bzr bzr files -> addFetcher "bzr" (addFiles files (toJSON bzr))
      Hg hg files -> addFetcher "hg" (addFiles files (toJSON hg))
      Darcs darcs files -> addFetcher "darcs" (addFiles files (toJSON darcs))
      Fossil fossil files -> addFetcher "fossil" (addFiles files (toJSON fossil))
      SVN svn files -> addFetcher "svn" (addFiles files (toJSON svn))
      CVS cvs files -> addFetcher "cvs" (addFiles files (toJSON cvs))
      Wiki wiki files -> addFetcher "wiki" (addFiles files (toJSON wiki))
    where
      addFetcher (Object obj) fetcher = Object (HM.insert "fetcher" fetcher obj)
      addFetcher other _ = other
      addFiles files (Object obj) = Object (HM.insert "files" (toJSON files) obj)
      addFiles _ other = other

fetchRecipes :: IO (HashMap Text Recipe)
fetchRecipes =
  get "http://melpa.org/recipes.json" $ \_ inp -> do
    parseResult <- parseEither parseJSON <$> S.parseFromStream json' inp
    case parseResult of
      Left err -> error err
      Right rcps -> return rcps
