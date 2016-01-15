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
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S

import Distribution.Nix.Pretty
import Util (runInteractiveProcess)

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Maybe Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { cvsRoot :: Text, cvsModule :: Maybe Text, sha256 :: Maybe Text }
           | Hg { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | SVN { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | GitHub { owner :: Text, repo :: Text, rev :: Text, sha256 :: Maybe Text }
           | GitLab { owner :: Text, repo :: Text, rev :: Text, sha256 :: Maybe Text }
           deriving Generic

importFetcher :: Fetch -> Doc
importFetcher (URL {}) = "fetchurl"
importFetcher (Git {}) = "fetchgit"
importFetcher (Bzr {}) = "fetchbzr"
importFetcher (CVS {}) = "fetchcvs"
importFetcher (Hg {}) = "fetchhg"
importFetcher (SVN {}) = "fetchsvn"
importFetcher (GitHub {}) = "fetchFromGitHub"
importFetcher (GitLab {}) = "fetchFromGitLab"

instance Pretty Fetch where
  pretty (URL {..}) = (fetchurl . attrs . catMaybes)
                      [ Just ("url", (dquotes . text) url)
                      , (,) "sha256" . (dquotes . text) <$> sha256
                      ]

  pretty (Git {..}) = (fetchgit . attrs . catMaybes)
                      [ Just ("url", (dquotes . text) url)
                      , Just ("rev", (dquotes . text) rev)
                      , (,) "sha256" . (dquotes . text) <$> sha256
                      , (,) "branchName" . (dquotes . text) <$> branchName
                      ]

  pretty (Bzr {..}) = (fetchbzr . attrs . catMaybes)
                      [ Just ("url", (dquotes . text) url)
                      , Just ("rev", (dquotes . text) rev)
                      , (,) "sha256" . (dquotes . text) <$> sha256
                      ]

  pretty (CVS {..}) = (fetchcvs . attrs . catMaybes)
                      [ Just ("cvsRoot", (dquotes . text) cvsRoot)
                      , (,) "module" . (dquotes . text) <$> cvsModule
                      , (,) "sha256" . (dquotes . text) <$> sha256
                      ]

  pretty (Hg {..}) = (fetchhg . attrs . catMaybes)
                     [ Just ("url", (dquotes . text) url)
                     , Just ("rev", (dquotes . text) rev)
                     , (,) "sha256" . (dquotes . text) <$> sha256
                     ]

  pretty (SVN {..}) = (fetchsvn . attrs . catMaybes)
                      [ Just ("url", (dquotes . text) url)
                      , Just ("rev", (dquotes . text) rev)
                      , (,) "sha256" . (dquotes . text) <$> sha256
                      ]

  pretty (GitHub {..}) = (fetchFromGitHub . attrs . catMaybes)
                         [ Just ("owner", (dquotes . text) owner)
                         , Just ("repo", (dquotes . text) repo)
                         , Just ("rev", (dquotes . text) rev)
                         , (,) "sha256" . (dquotes . text) <$> sha256
                         ]

  pretty (GitLab {..}) = (fetchFromGitLab . attrs . catMaybes)
                         [ Just ("owner", (dquotes . text) owner)
                         , Just ("repo", (dquotes . text) repo)
                         , Just ("rev", (dquotes . text) rev)
                         , (,) "sha256" . (dquotes . text) <$> sha256
                         ]

fetchOptions :: Options
fetchOptions = defaultOptions
               { constructorTagModifier = fetchTagModifier
               , sumEncoding = defaultTaggedObject
               , omitNothingFields = True
               , fieldLabelModifier = fetchLabelModifier
               }
  where
    fetchLabelModifier field =
      case field of
        "cvsModule" -> "module"
        _ -> field
    fetchTagModifier tag =
      case tag of
        "URL" -> "fetchurl"
        "Git" -> "fetchgit"
        "Bzr" -> "fetchbzr"
        "CVS" -> "fetchcvs"
        "Hg" -> "fetchhg"
        "SVN" -> "fetchsvn"
        "GitHub" -> "fetchFromGitHub"
        "GitLab" -> "fetchFromGitLab"
        _ -> error ("fetchOptions: unknown tag " ++ tag)

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

prefetch _ fetch@(GitHub {..}) = do
  let
    args = ["--url", T.unpack url, "--name", T.unpack name]
    url = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> rev <> ".tar.gz"
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-zip" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(GitLab {..}) = do
  let
    args = ["--url", T.unpack url, "--name", T.unpack name]
    url = "https://gitlab.com/" <> owner <> "/" <> repo
          <> "/repository/archive.tar.gz?ref=" <> rev
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-zip" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
