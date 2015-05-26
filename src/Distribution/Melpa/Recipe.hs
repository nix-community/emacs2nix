{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
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

import Paths_emacs2nix (getDataFileName)

data Recipe =
  forall f. (FromJSON f, ToJSON f) => Recipe
  { fetcher :: Fetcher f
  , recipe :: f
  , sha256 :: Maybe Text
  }

instance FromJSON Recipe where
  parseJSON = withObject "fetcher" $ \obj -> do
    fetch <- obj .: "fetcher"
    let obj_ = Object obj
    sha256 <- obj .:? "sha256"
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
  toJSON Recipe {..} = addHash (toJSON recipe)
    where
      addHash (Object obj) = Object (maybe id (HM.insert "sha256") (String <$> sha256) obj)
      addHash _ = error "the impossible happened"

readRecipes :: FilePath -> IO (Map Text Recipe)
readRecipes melpaDir = do
  let packageBuildEl = melpaDir </> "package-build.el"
      recipesDir = melpaDir </> "recipes"
  dumpRecipesEl <- getDataFileName "dump-recipes.el"
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", dumpRecipesEl
             , "-f", "dump-recipes-json", recipesDir
             ]
  bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           result <- parseEither parseJSON <$> S.parseFromStream json' out
           either error return result)
