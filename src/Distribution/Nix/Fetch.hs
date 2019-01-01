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

module Distribution.Nix.Fetch where

import Control.Error
import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Nix.Expr
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import Data.Aeson ( (.:!), withObject )
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types ( parseEither )

import Process

import qualified Distribution.Emacs.Name as Emacs

data Url =
    Url
        { url :: Text
        , sha256 :: Maybe Text
        , name :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)

data Git =
    Git
        { url :: Text
        , rev :: Text
        , branchName :: Maybe Text
        , sha256 :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)


data Hg =
    Hg
        { url :: Text
        , rev :: Text
        , sha256 :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)


data GitHub =
    GitHub
        { owner :: Text
        , repo :: Text
        , rev :: Text
        , sha256 :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)


data GitLab =
    GitLab
        { owner :: Text
        , repo :: Text
        , rev :: Text
        , sha256 :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)


data Recipe =
    Recipe
        { ename :: !Emacs.Name
        , rev :: !Text
        , sha256 :: Maybe Text
        }
  deriving (Data, Eq, Generic, Ord, Typeable)


data Fetch where
    FetchUrl :: Url -> Fetch
    FetchGit :: Git -> Fetch
    FetchHg :: Hg -> Fetch
    FetchGitHub :: GitHub -> Fetch
    FetchGitLab :: GitLab -> Fetch
    FetchRecipe :: Recipe -> Fetch


fetchUrl :: Url -> Fetch
fetchUrl = FetchUrl

fetchGit :: Git -> Fetch
fetchGit = FetchGit

fetchHg :: Hg -> Fetch
fetchHg = FetchHg

fetchGitHub :: GitHub -> Fetch
fetchGitHub = FetchGitHub

fetchGitLab :: GitLab -> Fetch
fetchGitLab = FetchGitLab

fetchRecipe :: Recipe -> Fetch
fetchRecipe = FetchRecipe

fetchExpr :: Fetch -> NExpr
fetchExpr (FetchUrl Url {..}) =
    (mkNonRecSet . catMaybes)
        [ (pure . bindTo "fetcher") (mkStr "url")
        , (pure . bindTo "url") (mkStr url)
        , bindTo "sha256" . mkStr <$> sha256
        , bindTo "name" . mkStr <$> name
        ]
fetchExpr (FetchGit Git {..}) =
    (mkNonRecSet . catMaybes)
        [ (pure . bindTo "fetcher") (mkStr "git")
        , (pure . bindTo "url") (mkStr url)
        , (pure . bindTo "rev") (mkStr rev)
        , bindTo "branchName" . mkStr <$> branchName
        , bindTo "sha256" . mkStr <$> sha256
        ]
fetchExpr (FetchHg Hg {..}) =
    (mkNonRecSet . catMaybes)
        [ (pure . bindTo "fetcher") (mkStr "hg")
        , (pure . bindTo "url") (mkStr url)
        , (pure . bindTo "rev") (mkStr rev)
        , bindTo "sha256" . mkStr <$> sha256
        ]
fetchExpr (FetchGitHub GitHub {..}) =
    (mkNonRecSet . catMaybes)
        [ (pure . bindTo "fetcher") (mkStr "github")
        , (pure . bindTo "owner") (mkStr owner)
        , (pure . bindTo "repo") (mkStr repo)
        , (pure . bindTo "rev") (mkStr rev)
        , bindTo "sha256" . mkStr <$> sha256
        ]
fetchExpr (FetchGitLab GitLab {..}) =
    (mkNonRecSet . catMaybes)
        [ (pure . bindTo "fetcher") (mkStr "gitlab")
        , (pure . bindTo "owner") (mkStr owner)
        , (pure . bindTo "repo") (mkStr repo)
        , (pure . bindTo "rev") (mkStr rev)
        , bindTo "sha256" . mkStr <$> sha256
        ]
fetchExpr (FetchRecipe Recipe {..}) =
    (mkNonRecSet . catMaybes)
        [ pure ("fetcher" $= mkStr "recipe")
        , pure ("ename" $= mkStr (Emacs.fromName ename))
        , pure ("rev" $= mkStr rev)
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

prefetch _ (FetchUrl fetch) = do
  let args = [T.unpack url]
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) ->
        pure (T.unpack path, fetchUrl fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
  where
    Url {..} = fetch

prefetch _ (FetchGit fetch) = do
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
      (Right sha, (_:path:_)) ->
        pure (T.unpack path, fetchGit fetch { sha256 = sha })
      _ -> throwIO BadPrefetchOutput
  where
    Git {..} = fetch

prefetch _ (FetchHg fetch) = do
  let args = [T.unpack url, T.unpack rev]
  prefetchHelper "nix-prefetch-hg" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) ->
        pure (T.unpack path, fetchHg fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
  where
    Hg {..} = fetch

prefetch _ (FetchGitHub fetch) = do
  let
    args = ["--name", T.unpack name, "--unpack", T.unpack url]
    url = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> rev <> ".tar.gz"
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) ->
        pure (T.unpack path, fetchGitHub fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
  where
    GitHub {..} = fetch

prefetch _ (FetchGitLab fetch) = do
  let
    args = ["--name", T.unpack name, "--unpack", T.unpack url]
    url = "https://gitlab.com/" <> owner <> "/" <> repo
          <> "/repository/archive.tar.gz?ref=" <> rev
    name = repo <> "-" <> rev <> "-src"
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) ->
        pure (T.unpack path, fetchGitLab fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
  where
    GitLab {..} = fetch

prefetch _ (FetchRecipe fetch) = do
  let args = [T.unpack url]
  prefetchHelper "nix-prefetch-url" args $ \out -> do
    hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case hashes of
      (hash:path:_) ->
        pure (T.unpack path, fetchRecipe fetch { sha256 = Just hash })
      _ -> throwIO BadPrefetchOutput
  where
    Recipe {..} = fetch
    tname = Emacs.fromName ename
    url =
        T.concat
            [ "https://raw.githubusercontent.com/melpa/melpa/"
            , rev
            , "/recipes/"
            , tname
            ]
