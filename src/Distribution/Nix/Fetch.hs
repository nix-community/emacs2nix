{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Nix.Types
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S

import Util (runInteractiveProcess)

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Maybe Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { cvsRoot :: Text, cvsModule :: Maybe Text, sha256 :: Maybe Text }
           | Hg { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | SVN { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | GitHub { owner :: Text, repo :: Text, rev :: Text, sha256 :: Maybe Text }
           | GitLab { owner :: Text, repo :: Text, rev :: Text, sha256 :: Maybe Text }

importFetcher :: Fetch -> Text
importFetcher (URL {}) = "fetchurl"
importFetcher (Git {}) = "fetchgit"
importFetcher (Bzr {}) = "fetchbzr"
importFetcher (CVS {}) = "fetchcvs"
importFetcher (Hg {}) = "fetchhg"
importFetcher (SVN {}) = "fetchsvn"
importFetcher (GitHub {}) = "fetchFromGitHub"
importFetcher (GitLab {}) = "fetchFromGitLab"

fetchExpr :: Fetch -> NExpr
fetchExpr (URL {..}) = (mkApp (mkSym "fetchurl") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr DoubleQuoted url)
                       , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                       ]
fetchExpr (Git {..}) = (mkApp (mkSym "fetchgit") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr DoubleQuoted url)
                       , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                       , bindTo "branchName" . mkStr DoubleQuoted <$> branchName
                       , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                       ]
fetchExpr (Bzr {..}) = (mkApp (mkSym "fetchbzr") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr DoubleQuoted url)
                       , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                       , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                       ]
fetchExpr (CVS {..}) = (mkApp (mkSym "fetchcvs") . mkNonRecSet . catMaybes)
                       [ Just ("cvsRoot" `bindTo` mkStr DoubleQuoted cvsRoot)
                       , bindTo "module" . mkStr DoubleQuoted <$> cvsModule
                       , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                       ]
fetchExpr (Hg {..}) = (mkApp (mkSym "fetchhg") . mkNonRecSet . catMaybes)
                      [ Just ("url" `bindTo` mkStr DoubleQuoted url)
                      , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                      , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                      ]
fetchExpr (SVN {..}) = (mkApp (mkSym "fetchsvn") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr DoubleQuoted url)
                       , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                       , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                       ]
fetchExpr (GitHub {..}) = (mkApp (mkSym "fetchFromGitHub") . mkNonRecSet . catMaybes)
                          [ Just ("owner" `bindTo` mkStr DoubleQuoted owner)
                          , Just ("repo" `bindTo` mkStr DoubleQuoted repo)
                          , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                          , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                          ]
fetchExpr (GitLab {..}) = (mkApp (mkSym "fetchFromGitLab") . mkNonRecSet . catMaybes)
                          [ Just ("owner" `bindTo` mkStr DoubleQuoted owner)
                          , Just ("repo" `bindTo` mkStr DoubleQuoted repo)
                          , Just ("rev" `bindTo` mkStr DoubleQuoted rev)
                          , bindTo "sha256" . mkStr DoubleQuoted <$> sha256
                          ]

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
