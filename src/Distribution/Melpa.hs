{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa (Melpa(..), getMelpa, readMelpa) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.QSem
import Control.Error hiding (err)
import Control.Exception (SomeException(..), bracket, finally, handle)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (defaultOptions, parseMaybe)
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Package (Package, getPackage)
import Distribution.Melpa.Recipe (Recipe, readRecipes)
import qualified Distribution.Melpa.Recipe as Recipe

data Melpa = Melpa
             { rev :: Text
             , sha256 :: Text
             , recipes :: Map Text Recipe
             , packages :: Map Text Package
             }
           deriving Generic

instance ToJSON Melpa where
  toJSON = genericToJSON defaultOptions

instance FromJSON Melpa where
  parseJSON = genericParseJSON defaultOptions

readMelpa :: FilePath -> IO (Maybe Melpa)
readMelpa packagesJson =
  handle
  (\(SomeException _) -> return Nothing)
  (S.withFileAsInput packagesJson $ \inp ->
       parseMaybe parseJSON <$> S.parseFromStream json' inp)

getMelpa :: FilePath -> FilePath -> Maybe Melpa -> IO (Either Text Melpa)
getMelpa melpaDir workDir oldMelpa = runEitherT $ do
  rev <- getRev_Melpa melpaDir
  sha256 <- getFileHash melpaDir "package-build.el"

  recipes <- M.traverseWithKey (getRecipeHash melpaDir) =<< liftIO (readRecipes melpaDir)

  qsem <- liftIO (getNumCapabilities >>= newQSem)

  let oldPackages = maybe M.empty packages oldMelpa

  liftIO (createDirectoryIfMissing True workDir)

  let getPackage_ name rcp = Concurrently $ do
        waitQSem qsem
        getPackage melpaDir workDir oldPackages name rcp `finally` signalQSem qsem
  (errors, M.fromList -> packages) <-
    partitionEithers . map liftEither . M.toList
    <$> liftIO (runConcurrently (M.traverseWithKey getPackage_ recipes))
  for_ errors $ \(name, err) -> liftIO (T.putStrLn (name <> ": " <> err))

  return Melpa {..}
  where
    liftEither (name, stat) = either (Left . (,) name) (Right . (,) name) stat

getRev_Melpa :: FilePath -> EitherT Text IO Text
getRev_Melpa melpaDir = EitherT $ do
  let args = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
  bracket
    (S.runInteractiveProcess "git" args (Just melpaDir) Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
         S.write Nothing inp
         revs <- S.toList =<< S.decodeUtf8 =<< S.lines out
         return (headErr "could not get revision" revs))

getRecipeHash :: FilePath -> Text -> Recipe -> EitherT Text IO Recipe
getRecipeHash melpaDir name recipe = do
  sha256_ <- getFileHash melpaDir ("recipes" </> T.unpack name)
  return recipe { Recipe.sha256 = Just sha256_ }

getFileHash :: FilePath -> FilePath -> EitherT Text IO Text
getFileHash melpaDir path = EitherT $ do
  let args = [ "--base32", "--type", "sha256", "--flat", melpaDir </> path ]
  bracket
    (S.runInteractiveProcess "nix-hash" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
         S.write Nothing inp
         sha256s <- S.toList =<< S.decodeUtf8 =<< S.lines out
         return $ headErr "could not calculate hash" sha256s)
