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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa ( updateMelpa ) where

import Control.Concurrent ( getNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Concurrent.QSem
import Control.Error hiding ( err )
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson ( parseJSON )
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types ( parseEither )
import Data.Char ( isDigit, isHexDigit )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Monoid ( (<>) )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import System.Directory
       ( createDirectoryIfMissing, copyFile, doesFileExist )
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp ( withSystemTempDirectory )

import Distribution.Melpa.Recipe
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Hash as Nix
import Distribution.Nix.Index
import qualified Distribution.Nix.Name as Nix
import qualified Distribution.Nix.Package.Melpa as Recipe ( Recipe(..) )
import qualified Distribution.Nix.Package.Melpa as Nix

import Paths_emacs2nix ( getDataFileName )
import Util

data Melpa = Melpa { melpaDir :: FilePath
                   , melpaCommit :: Text
                   }

data ParseMelpaError = ParseMelpaError String
  deriving (Show, Typeable)

instance Exception ParseMelpaError

updateMelpa :: FilePath
            -> Bool
            -> FilePath  -- ^ temporary workspace
            -> FilePath  -- ^ dump MELPA recipes here
            -> Bool  -- ^ only generate the index
            -> Set Text
            -> IO ()
updateMelpa melpaDir stable workDir melpaOut indexOnly packages = do
  melpaCommit <- revision_Git Nothing melpaDir
  let melpa = Melpa {..}

  recipes <- readRecipes melpaDir

  let
    isSelected name = Set.null packages || Set.member name packages
    selected = M.filterWithKey (\k _ -> isSelected k) recipes

  createDirectoryIfMissing True workDir

  sem <- newQSem =<< getNumCapabilities

  let update pkg
        = Concurrently
          (bracket (waitQSem sem) (\_ -> signalQSem sem)
            (\_ -> getPackage melpa stable workDir pkg))
      toExpression pkg = (Nix.pname pkg, Nix.expression pkg)
  updates <- if indexOnly
             then pure M.empty
             else M.fromList . map toExpression . catMaybes
                  <$> runConcurrently (traverse update (M.toList selected))

  existing <- readIndex melpaOut

  let
    recipeDeleted (Nix.fromName -> name)
      = M.notMember name recipes && isSelected name
    updated
      = M.union
        updates
        (M.filterWithKey (\k _ -> not (recipeDeleted k)) existing)

  writeIndex melpaOut updated

data PackageException = PackageException Text SomeException
  deriving (Show, Typeable)

instance Exception PackageException

getPackage :: Melpa -> Bool -> FilePath -> (Text, Recipe)
           -> IO (Maybe Nix.Package)
getPackage melpa@(Melpa {..}) stable workDir (name, recipe)
  = showExceptions $ mapExceptionIO (PackageException name) $ do
    let
      packageBuildEl = melpaDir </> "package-build" </> "package-build.el"
      recipeFile = melpaDir </> recipeFileName name
      sourceDir = workDir </> T.unpack name
    melpaRecipe <- getRecipe melpa name
    version <- getVersion packageBuildEl stable recipeFile name sourceDir
    fetch0 <- getFetcher name sourceDir recipe
    (path, fetch) <- Nix.prefetch name fetch0
    deps <- M.keys <$> getDeps packageBuildEl recipeFile name path
    pure Nix.Package { Nix.pname = Nix.fromText name
                     , Nix.version = version
                     , Nix.fetch = fetch
                     , Nix.deps = map Nix.fromText deps
                     , Nix.recipe = melpaRecipe
                     }

recipeFileName :: Text -> FilePath
recipeFileName (T.unpack -> name) = "recipes" </> name

getRecipe :: Melpa -> Text -> IO Nix.Recipe
getRecipe (Melpa {..}) name = do
  let
      rcp = recipeFileName name
  hash <- Nix.hash (melpaDir </> rcp)
  commit <- let args = [ "log"
                       , "--first-parent"
                       , "-n", "1"
                       , "--pretty=format:%H"
                       , "--", rcp
                       ]
                getRecipeRevision out = do
                  revs <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
                  case revs of
                    (rev:_) -> pure rev
                    _ -> throwIO NoRevision
            in runInteractiveProcess "git" args (Just melpaDir) Nothing
               getRecipeRevision
  pure Nix.Recipe { Recipe.ename = name
                  , Recipe.commit = commit
                  , Recipe.sha256 = hash
                  }

data DarcsFetcherNotImplemented = DarcsFetcherNotImplemented
  deriving (Show, Typeable)

instance Exception DarcsFetcherNotImplemented

data FossilFetcherNotImplemented = FossilFetcherNotImplemented
  deriving (Show, Typeable)

instance Exception FossilFetcherNotImplemented

getFetcher :: Text -> FilePath -> Recipe -> IO Nix.Fetch

getFetcher _ sourceDir Bzr {..} = do
  rev <- revision_Bzr sourceDir
  pure Nix.Bzr { Nix.url = url
               , Nix.rev = rev
               , Nix.sha256 = Nothing
               }

getFetcher _ sourceDir Git {..} = do
  rev <- revision_Git branch sourceDir
  pure Nix.Git { Nix.url = url
               , Nix.rev = rev
               , Nix.branchName = branch
               , Nix.sha256 = Nothing
               }

getFetcher _ sourceDir GitHub {..} = do
  rev <- revision_Git branch sourceDir
  let (owner:rest) = T.splitOn "/" repo
  pure Nix.GitHub { Nix.owner = owner
                  , Nix.repo = T.concat rest
                  , Nix.rev = rev
                  , Nix.sha256 = Nothing
                  }

getFetcher _ sourceDir GitLab {..} = do
  rev <- revision_Git branch sourceDir
  let (owner:rest) = T.splitOn "/" repo
  pure Nix.GitLab { Nix.owner = owner
                  , Nix.repo = T.concat rest
                  , Nix.rev = rev
                  , Nix.sha256 = Nothing
                  }

getFetcher _ _ CVS {..} =
  pure Nix.CVS { Nix.cvsRoot = url
               , Nix.cvsModule = cvsModule
               , Nix.sha256 = Nothing
               }

getFetcher _ sourceDir Hg {..} = do
  rev <- revision_Hg sourceDir
  pure Nix.Hg { Nix.url = url
              , Nix.rev = rev
              , Nix.sha256 = Nothing
              }

getFetcher _ sourceDir Bitbucket {..} = do
  let url = "https://bitbucket.com/" <> repo
  rev <- revision_Hg sourceDir
  pure Nix.Hg { Nix.url = url
              , Nix.rev = rev
              , Nix.sha256 = Nothing
              }

getFetcher _ sourceDir SVN {..} = do
  rev <- revision_SVN sourceDir
  pure Nix.SVN { Nix.url = url
               , Nix.rev = rev
               , Nix.sha256 = Nothing
               }

getFetcher name _ Wiki {..} = do
  let
    url = fromMaybe defaultUrl wikiUrl
    defaultUrl = "https://www.emacswiki.org/emacs/download/" <> name <> ".el"
  pure Nix.URL { Nix.url = url
               , Nix.sha256 = Nothing
               }

getFetcher _ _ Darcs {..} = throwIO DarcsFetcherNotImplemented

getFetcher _ _ Fossil {..} = throwIO FossilFetcherNotImplemented

data NoVersion = NoVersion
  deriving (Show, Typeable)

instance Exception NoVersion

getVersion :: FilePath -> Bool -> FilePath -> Text -> FilePath -> IO Text
getVersion packageBuildEl stable recipeFile packageName sourceDir
  = do
    checkoutEl <- getDataFileName "checkout.el"
    let args = [ "-Q"
                , "--batch"
                , "-l", packageBuildEl
                , "-l", checkoutEl
                , "-f", if stable then "checkout-stable" else "checkout"
                , recipeFile, T.unpack packageName, sourceDir
                ]
    runInteractiveProcess "emacs" args Nothing Nothing $ \out -> do
      result <- liftIO (S.fold (<>) Nothing =<< S.map Just =<< S.decodeUtf8 out)
      case result of
        Nothing -> throwIO NoVersion
        Just ver
          | ver == "nil" -> throwIO NoVersion
          | otherwise -> pure ver

data ParseDepsError = ParseDepsError String
  deriving (Show, Typeable)

instance Exception ParseDepsError

getDeps :: FilePath -> FilePath -> Text -> FilePath -> IO (Map Text [Integer])
getDeps packageBuildEl recipeFile packageName sourceDirOrEl
  = do
    getDepsEl <- getDataFileName "get-deps.el"
    isEl <- doesFileExist sourceDirOrEl
    let withSourceDir act
          | isEl = do
              let tmpl = "melpa2nix-" <> T.unpack packageName
                  elFile = T.unpack packageName <.> "el"
              withSystemTempDirectory tmpl $ \sourceDir -> do
                copyFile sourceDirOrEl (sourceDir </> elFile)
                act sourceDir
          | otherwise = act sourceDirOrEl
    withSourceDir $ \sourceDir -> do
      let args = [ "-Q"
                  , "--batch"
                  , "-l", packageBuildEl
                  , "-l", getDepsEl
                  , "-f", "get-deps", recipeFile, T.unpack packageName, sourceDir
                  ]
      runInteractiveProcess "emacs" args Nothing Nothing $ \out -> do
        result <- liftIO (parseEither parseJSON <$> S.parseFromStream json' out)
        case result of
          Left err -> throwIO (ParseDepsError err)
          Right deps_ -> pure deps_

data NoRevision = NoRevision
  deriving (Show, Typeable)

instance Exception NoRevision

revision_Bzr :: FilePath -> IO Text
revision_Bzr tmp = do
  let args = [ "log", "-l1", tmp ]
  runInteractiveProcess "bzr" args Nothing Nothing $ \out -> do
    let getRevno = (T.takeWhile isDigit . T.strip <$>) . T.stripPrefix "revno:"
    revnos <- mapMaybe getRevno <$> liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case revnos of
      (rev:_) -> pure rev
      _ -> throwIO NoRevision

revision_Git :: Maybe Text -> FilePath -> IO Text
revision_Git branch tmp = do
  runInteractiveProcess "git" gitArgs (Just tmp) Nothing $ \out -> do
    revs <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    case revs of
      (rev:_) -> pure rev
      _ -> throwIO NoRevision
  where
    fullBranch = do
        branch_ <- branch
        -- package-build does not fetch all branches by default, so they must be referred
        -- to under the origin/ prefix
        return (T.unpack ("origin/" <> branch_))
    gitArgs = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
              ++ maybeToList fullBranch

revision_Hg :: FilePath -> IO Text
revision_Hg tmp = do
  runInteractiveProcess "hg" ["tags"] (Just tmp) Nothing $ \out -> do
    lines_ <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    let revs = catMaybes (hgRev <$> lines_)
    case revs of
      (rev:_) -> pure rev
      _ -> throwIO NoRevision
  where
    hgRev txt = do
        afterTip <- T.strip <$> T.stripPrefix "tip" txt
        let (_, T.strip . T.takeWhile isHexDigit . T.drop 1 -> rev) = T.breakOn ":" afterTip
        if T.null rev
          then Nothing
          else return rev

revision_SVN :: FilePath -> IO Text
revision_SVN tmp = do
  runInteractiveProcess "svn" ["info"] (Just tmp) Nothing $ \out -> do
    lines_ <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
    let revs = catMaybes (svnRev <$> lines_)
    case revs of
      (rev:_) -> pure rev
      _ -> throwIO NoRevision
  where
    svnRev = fmap T.strip . T.stripPrefix "Revision:"
