{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Control.Error
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Recipe (Recipe(Recipe))
import qualified Distribution.Melpa.Recipe as Recipe
import Distribution.Melpa.Version

import Paths_melpa2nix (getDataFileName)

data Package =
  forall f. (FromJSON f, ToJSON f) => Package
  { ver :: Text
  , deps :: [Text]
  , fetcher :: Fetcher f
  , recipe :: f
  , rev :: Text
  , hash :: Text
  }

instance FromJSON Package where
  parseJSON = withObject "package" $ \obj -> do
    ver <- obj .: "ver"
    deps <- obj .: "deps"
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
    , "deps" .= deps
    , "recipe" .= Recipe { Recipe.fetcher = fetcher, Recipe.recipe = recipe }
    , "rev" .= rev
    , "hash" .= hash
    ]

getPackage :: FilePath -> FilePath -> Text -> Recipe -> IO (Either Text Package)
getPackage packageBuildEl recipesEl packageName rcp =
  case rcp of
    Recipe { Recipe.fetcher = fetcher, Recipe.recipe = recipe } -> runEitherT $ do
      (ver, rev) <- EitherT $ withTemp $ \tmp -> runEitherT $ do
        ver <- getVersion packageBuildEl recipesEl packageName tmp
        rev <- getRev fetcher packageName recipe tmp
        return (ver, rev)
      (sourceDir, hash) <- prefetch fetcher packageName recipe rev
      deps <- getDeps packageBuildEl recipesEl packageName sourceDir
      return Package {..}
  where
    withTemp = withSystemTempDirectory ("melpa2nix-" <> T.unpack packageName)

getDeps :: FilePath -> FilePath -> Text -> FilePath -> EitherT Text IO [Text]
getDeps packageBuildEl recipesEl packageName sourceDir = EitherT $ do
  getDepsEl <- getDataFileName "get-deps.el"
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", getDepsEl
             , "-f", "get-deps", recipesEl, T.unpack packageName, sourceDir
             ]
  bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           result <- parseEither parseJSON <$> S.parseFromStream json' out
           either error (return . Right . M.keys) (result :: Either String (Map Text Version)))

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
