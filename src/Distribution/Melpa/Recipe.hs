{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Fetcher.Bzr
import Distribution.Melpa.Fetcher.CVS
import Distribution.Melpa.Fetcher.Darcs
import Distribution.Melpa.Fetcher.Fossil
import Distribution.Melpa.Fetcher.Git
import Distribution.Melpa.Fetcher.GitHub
import Distribution.Melpa.Fetcher.Hg
import Distribution.Melpa.Fetcher.SVN
import Distribution.Melpa.Fetcher.Wiki

data Recipe =
  forall f. (FromJSON f, ToJSON f) => Recipe
  { fetcher :: Fetcher f
  , recipe :: f
  }

instance FromJSON Recipe where
  parseJSON = withObject "fetcher" $ \obj -> do
    fetch <- obj .: "fetcher"
    let obj_ = Object obj
    case fetch of
      "git" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchGit
        return Recipe {..}
      "github" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchGitHub
        return Recipe {..}
      "bzr" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchBzr
        return Recipe {..}
      "hg" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchHg
        return Recipe {..}
      "darcs" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchDarcs
        return Recipe {..}
      "fossil" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchFossil
        return Recipe {..}
      "svn" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchSVN
        return Recipe {..}
      "cvs" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchCVS
        return Recipe {..}
      "wiki" -> do
        recipe <- parseJSON obj_
        let fetcher = fetchWiki
        return Recipe {..}
      unknown -> fail ("unknown fetcher '" ++ T.unpack unknown ++ "'")

instance ToJSON Recipe where
  toJSON Recipe {..} = toJSON recipe

readRecipes :: FilePath -> IO (HashMap Text Recipe)
readRecipes path =
  S.withFileAsInput path $ \inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result
