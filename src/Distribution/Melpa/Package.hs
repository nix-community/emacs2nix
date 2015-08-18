{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Control.Error
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (defaultOptions, parseEither, parseMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Directory (doesFileExist, copyFile)
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Recipe
import qualified Distribution.Melpa.Recipe as Recipe

import Paths_emacs2nix (getDataFileName)

data Package = Package
               { ver :: Text
               , rev :: Text
               , sha256 :: Text
               , deps :: [Text]
               }
             deriving Generic

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

readPackages :: FilePath -> IO (Map Text Package)
readPackages packagesJson =
    S.withFileAsInput packagesJson $ \inp -> do
        result <- parseMaybe parseJSON <$> S.parseFromStream json' inp
        return $ fromMaybe M.empty result

getPackage :: FilePath  -- ^ path to MELPA repository
           -> FilePath  -- ^ temporary workspace
           -> Text  -- ^ package name
           -> Recipe  -- ^ package recipe
           -> IO (Maybe Package)
getPackage melpaDir workDir packageName rcp =
  let packageBuildEl = melpaDir </> "package-build.el"
      recipeFile = melpaDir </> "recipes" </> T.unpack packageName
  in case rcp of
       Recipe { Recipe.fetcher = fetcher_, Recipe.recipe = recipe_ } -> runMaybeT $ do
         let tmp = workDir </> T.unpack packageName
         ver_ <- getVersion packageBuildEl recipeFile packageName tmp
         rev_ <- getRev fetcher_ packageName recipe_ tmp
         (path_, hash_) <- prefetch fetcher_ packageName recipe_ rev_
         deps_ <- getDeps packageBuildEl recipeFile packageName path_
         return
           Package
           { ver = ver_
           , rev = rev_
           , sha256 = hash_
           , deps = M.keys deps_
           }

getDeps :: FilePath -> FilePath -> Text -> FilePath
        -> MaybeT IO (Map Text [Integer])
getDeps packageBuildEl recipeFile packageName sourceDirOrEl = do
  getDepsEl <- liftIO $ getDataFileName "get-deps.el"
  isEl <- liftIO $ doesFileExist sourceDirOrEl
  let withSourceDir act
        | isEl = do
            let tmpl = "melpa2nix-" <> T.unpack packageName
                elFile = T.unpack packageName <.> "el"
            withSystemTempDirectory tmpl $ \sourceDir -> do
              copyFile sourceDirOrEl (sourceDir </> elFile)
              act sourceDir
        | otherwise = act sourceDirOrEl
  handleAll $ MaybeT $ withSourceDir $ \sourceDir -> do
    let args = [ "--batch"
               , "-l", packageBuildEl
               , "-l", getDepsEl
               , "-f", "get-deps", recipeFile, T.unpack packageName, sourceDir
               ]
    bracket
      (S.runInteractiveProcess "emacs" args Nothing Nothing)
      (\(_, _, _, pid) -> S.waitForProcess pid)
      (\(_, out, _, _) -> do
           result <- parseEither parseJSON <$> S.parseFromStream json' out
           let anyerr txt = "error parsing dependencies in "
                            <> T.pack sourceDir <> ":\n" <> txt
           case result of
             Left errmsg -> do
               S.write (Just $ anyerr $ T.pack errmsg) =<< S.encodeUtf8 S.stderr
               return Nothing
             Right deps_ -> return $ Just deps_)

getVersion :: FilePath -> FilePath -> Text -> FilePath -> MaybeT IO Text
getVersion packageBuildEl recipeFile packageName sourceDir = do
  checkoutEl <- liftIO (getDataFileName "checkout.el")
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", checkoutEl
             , "-f", "checkout", recipeFile, T.unpack packageName, sourceDir
             ]
  handleAll $ MaybeT $ bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(_, out, _, _) -> S.fold (<>) Nothing =<< S.map Just =<< S.decodeUtf8 out)
