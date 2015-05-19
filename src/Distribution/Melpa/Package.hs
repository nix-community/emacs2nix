{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Control.Error
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.IO.Streams as S
import System.FilePath

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Recipe (Recipe(Recipe))
import qualified Distribution.Melpa.Recipe as Recipe

import Paths_melpa2nix (getDataFileName)

data Package =
  forall f. (FromJSON f, ToJSON f) => Package
  { ver :: Text
  , fetcher :: Fetcher f
  , recipe :: f
  , rev :: Text
  , hash :: Text
  }

instance FromJSON Package where
  parseJSON = withObject "package" $ \obj -> do
    ver <- obj .: "ver"
    rcp <- obj .: "recipe"
    rev <- obj .: "rev"
    hash <- obj .: "hash"
    case rcp of
      Recipe { Recipe.recipe = recipe, Recipe.fetcher = fetcher } ->
        return Package {..}

instance ToJSON Package where
  toJSON Package {..} =
    object
    [ "ver" .= ver
    , "recipe" .= Recipe { Recipe.fetcher = fetcher, Recipe.recipe = recipe }
    , "rev" .= rev
    , "hash" .= hash
    ]

getPackage :: FilePath  -- ^ path to package-build.el
           -> FilePath  -- ^ path to recipes.el
           -> FilePath  -- ^ temporary workspace
           -> Text  -- ^ package name
           -> Recipe  -- ^ package recipe
           -> IO (Either Text Package)
getPackage packageBuildEl recipesEl workDir packageName rcp =
  case rcp of
    Recipe { Recipe.fetcher = fetcher, Recipe.recipe = recipe } -> runEitherT $ do
      let tmp = workDir </> T.unpack packageName
      ver <- getVersion packageBuildEl recipesEl packageName tmp
      rev <- getRev fetcher packageName recipe tmp
      hash <- prefetch fetcher packageName recipe rev
      return Package {..}

getVersion :: FilePath -> FilePath -> Text -> FilePath -> EitherT Text IO Text
getVersion packageBuildEl recipesEl packageName sourceDir = do
  checkoutEl <- liftIO $ getDataFileName "checkout.el"
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", checkoutEl
             , "-f", "checkout", recipesEl, T.unpack packageName, sourceDir
             ]
  EitherT $ bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           mver <- S.lines out >>= S.decodeUtf8 >>= S.read
           return $ case mver of
             Nothing -> Left (packageName <> ": could not determine version")
             Just ver -> Right ver)
