{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), defaultOptions, defaultTaggedObject
  , genericParseJSON, genericToJSON )
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S

import Util (runInteractiveProcess)

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Maybe Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { cvsRoot :: Text, cvsModule :: Maybe Text, sha256 :: Maybe Text }
           | Hg { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | SVN { url :: Text, rev :: Text, sha256 :: Maybe Text }
           deriving Generic

fetchOptions :: Options
fetchOptions = defaultOptions
               { constructorTagModifier = ("fetch" ++) . map Char.toLower
               , sumEncoding = defaultTaggedObject
               , omitNothingFields = True
               , fieldLabelModifier = fetchLabelModifier
               }
  where
    fetchLabelModifier field =
      case field of
        "cvsModule" -> "module"
        _ -> field

instance FromJSON Fetch where
  parseJSON = genericParseJSON fetchOptions

instance ToJSON Fetch where
  toJSON = genericToJSON fetchOptions

newtype FetchError = FetchError SomeException
  deriving (Show, Typeable)

instance Exception FetchError

prefetchHelper :: String -> [String]
               -> (S.InputStream ByteString -> IO a)
               -> IO a
prefetchHelper fetcher args go = mapException FetchError helper
  where
    helper = do
      env <- addToEnv "PRINT_PATH" "1"
      runInteractiveProcess fetcher args Nothing (Just env) go

addToEnv :: MonadIO m => String -> String -> m [(String, String)]
addToEnv var val = liftIO $ M.toList . M.insert var val . M.fromList <$> getEnvironment

data BadPrefetchOutput = BadPrefetchOutput
  deriving (Show, Typeable)

instance Exception BadPrefetchOutput

prefetch :: Text -> Fetch -> IO (FilePath, Fetch)

prefetch _ fetch@(URL {..}) = do
  let args = [T.unpack url]
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(Git {..}) = do
  let
    args = [ "--fetch-submodules"
           , "--url", T.unpack url, "--rev", T.unpack rev
           ] ++ fromMaybe [] branch
    branch = do
      name <- branchName
      pure ["--branch-name", T.unpack name]
  prefetchHelper "nix-prefetch-git" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(Bzr {..}) = do
  let args = [T.unpack url, T.unpack rev]
  prefetchHelper "nix-prefetch-bzr" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(Hg {..}) = do
  let args = [T.unpack url, T.unpack rev]
  prefetchHelper "nix-prefetch-hg" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch name fetch@(CVS {..}) = do
  let args = [T.unpack cvsRoot, T.unpack (fromMaybe name cvsModule)]
  prefetchHelper "nix-prefetch-cvs" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(SVN {..}) = do
  let args = [T.unpack url, T.unpack rev]
  prefetchHelper "nix-prefetch-svn" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
