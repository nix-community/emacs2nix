{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Control.Error
import Control.Exception (SomeException)
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
import GHC.Generics
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S

import Util (runInteractiveProcess)

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Maybe Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { url :: Text, cvsModule :: Maybe Text, sha256 :: Maybe Text }
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

data FetchError = HashParseFetchError
                | OtherFetchError SomeException
                | NixPrefetchError Int Text
  deriving (Show)

runPrefetch :: String -> [String]
            -> (S.InputStream ByteString -> ExceptT FetchError IO a)
            -> ExceptT FetchError IO a
runPrefetch fetcher args go = do
  env <- liftIO (addToEnv "PRINT_PATH" "1")
  runInteractiveProcess fetcher args Nothing (Just env) OtherFetchError NixPrefetchError go

addToEnv :: MonadIO m => String -> String -> m [(String, String)]
addToEnv var val = liftIO $ M.toList . M.insert var val . M.fromList <$> getEnvironment

prefetch :: Text -> Fetch -> ExceptT FetchError IO (FilePath, Fetch)

prefetch _ fetch@(URL {..}) = do
  let args = [T.unpack url]
  runPrefetch "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError

prefetch _ fetch@(Git {..}) = do
  let args = ["--url", T.unpack url, "--rev", T.unpack rev]
             ++ fromMaybe [] (do name <- branchName
                                 return ["--branch-name", T.unpack name])
  runPrefetch "nix-prefetch-git" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError

prefetch _ fetch@(Bzr {..}) = do
  let args = [T.unpack url, T.unpack rev]
  runPrefetch "nix-prefetch-bzr" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError

prefetch _ fetch@(Hg {..}) = do
  let args = [T.unpack url, T.unpack rev]
  runPrefetch "nix-prefetch-hg" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError

prefetch name fetch@(CVS {..}) = do
  let args = [T.unpack url, T.unpack (fromMaybe name cvsModule)]
  runPrefetch "nix-prefetch-cvs" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError

prefetch _ fetch@(SVN {..}) = do
  let args = [T.unpack url, T.unpack rev]
  runPrefetch "nix-prefetch-svn" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (_:hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwE HashParseFetchError
