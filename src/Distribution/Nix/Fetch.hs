{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2016 Thomas Tuegel

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Nix.Expr
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import Data.Aeson ( (.:!), withObject )
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types ( parseEither )

import Process

data Fetch = URL { url :: Text, sha256 :: Maybe Text, name :: Maybe Text }
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
fetchExpr (URL {..}) = ((@@) (mkSym "fetchurl") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr url)
                       , bindTo "sha256" . mkStr <$> sha256
                       , bindTo "name" . mkStr <$> name
                       ]
fetchExpr (Git {..}) = ((@@) (mkSym "fetchgit") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr url)
                       , Just ("rev" `bindTo` mkStr rev)
                       , bindTo "branchName" . mkStr <$> branchName
                       , bindTo "sha256" . mkStr <$> sha256
                       ]
fetchExpr (Bzr {..}) = ((@@) (mkSym "fetchbzr") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr url)
                       , Just ("rev" `bindTo` mkStr rev)
                       , bindTo "sha256" . mkStr <$> sha256
                       ]
fetchExpr (CVS {..}) = ((@@) (mkSym "fetchcvs") . mkNonRecSet . catMaybes)
                       [ Just ("cvsRoot" `bindTo` mkStr cvsRoot)
                       , bindTo "module" . mkStr <$> cvsModule
                       , bindTo "sha256" . mkStr <$> sha256
                       ]
fetchExpr (Hg {..}) = ((@@) (mkSym "fetchhg") . mkNonRecSet . catMaybes)
                      [ Just ("url" `bindTo` mkStr url)
                      , Just ("rev" `bindTo` mkStr rev)
                      , bindTo "sha256" . mkStr <$> sha256
                      ]
fetchExpr (SVN {..}) = ((@@) (mkSym "fetchsvn") . mkNonRecSet . catMaybes)
                       [ Just ("url" `bindTo` mkStr url)
                       , Just ("rev" `bindTo` mkStr rev)
                       , bindTo "sha256" . mkStr <$> sha256
                       ]
fetchExpr (GitHub {..}) = ((@@) (mkSym "fetchFromGitHub") . mkNonRecSet . catMaybes)
                          [ Just ("owner" `bindTo` mkStr owner)
                          , Just ("repo" `bindTo` mkStr repo)
                          , Just ("rev" `bindTo` mkStr rev)
                          , bindTo "sha256" . mkStr <$> sha256
                          ]
fetchExpr (GitLab {..}) = ((@@) (mkSym "fetchFromGitLab") . mkNonRecSet . catMaybes)
                          [ Just ("owner" `bindTo` mkStr owner)
                          , Just ("repo" `bindTo` mkStr repo)
                          , Just ("rev" `bindTo` mkStr rev)
                          , bindTo "sha256" . mkStr <$> sha256
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
      env <- addToEnv [("PRINT_PATH", "1")]
      runInteractiveProcess fetcher args Nothing (Just env) go

addToEnv :: [(String, String)] -> IO [(String, String)]
addToEnv env = (++ env) <$> getEnvironment

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
    jsonp = withObject "need an object" (\o -> o .:! "sha256")
  prefetchHelper "nix-prefetch-git" args $ \out -> do
    sha256_ <- liftIO $ parseEither jsonp <$> S.parseFromStream json' out
    pathes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case (sha256_, pathes) of
      (Right sha, (_:path:_)) -> pure (T.unpack path, fetch { sha256 = sha })
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
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
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
    args = ["--name", T.unpack name, "--unpack", T.unpack url]
    url = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> rev <> ".tar.gz"
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput

prefetch _ fetch@(GitLab {..}) = do
  let
    args = ["--name", T.unpack name, "--unpack", T.unpack url]
    url = "https://gitlab.com/" <> owner <> "/" <> repo
          <> "/repository/archive.tar.gz?ref=" <> rev
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) -> pure (T.unpack path, fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
