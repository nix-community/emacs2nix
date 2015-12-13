{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa (getMelpa, readMelpa) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.QSem
import Control.Error hiding (err)
import Control.Exception (SomeException(..), bracket, handle)
import Control.Monad.IO.Class
import Data.Aeson (parseJSON)
import Data.Aeson.Parser (json')
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Char (isHexDigit)
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.FilePath
import System.IO (hPutStrLn, stderr)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Melpa.Recipe
import Distribution.Nix.Fetch (FetchError)
import qualified Distribution.Nix.Fetch as Nix
import Distribution.Nix.Hash (HashError)
import qualified Distribution.Nix.Hash as Nix
import qualified Distribution.Nix.Package.Melpa as Recipe ( Recipe(..) )
import qualified Distribution.Nix.Package.Melpa as Nix

import Paths_emacs2nix (getDataFileName)
import Util (runInteractiveProcess)

readMelpa :: FilePath -> IO (Maybe (Map Text Nix.Package))
readMelpa packagesJson =
  handle
  (\(SomeException _) -> return Nothing)
  (S.withFileAsInput packagesJson $ \inp ->
       parseMaybe parseJSON <$> S.parseFromStream json' inp)

getMelpa :: Int
         -> FilePath
         -> Bool
         -> FilePath
         -> Map Text Nix.Package
         -> Set Text
         -> IO (Map Text Nix.Package)
getMelpa nthreads melpaDir stable workDir oldPackages packages = do
  Right melpaCommit <- runExceptT $ revision_Git Nothing melpaDir

  recipes <- readRecipes melpaDir

  createDirectoryIfMissing True workDir

  sem <- (if nthreads > 0 then pure nthreads else getNumCapabilities) >>= newQSem
  let getPackage_ name recipe
        | Set.null packages || Set.member name packages
          = getPackage sem melpaDir melpaCommit stable workDir name recipe
        | otherwise
          = return Nothing
      getPackages = M.traverseWithKey getPackage_ recipes
  newPackages <- catMaybesMap <$> runConcurrently getPackages

  (return . Nix.cleanNames) (newPackages <> oldPackages)
  where
    catMaybesMap = M.fromList . mapMaybe liftMaybe . M.toList
    liftMaybe (x, y) = (,) x <$> y

data PackageError = HashRecipePackageError HashError
                  | FetchPackageError FetchError
                  | VersionPackageError VersionError
                  | RevisionPackageError RevisionError
                  | DepsPackageError DepsError
  deriving (Show)

getPackage :: QSem -> FilePath -> Text -> Bool -> FilePath -> Text -> Recipe
           -> Concurrently (Maybe Nix.Package)
getPackage sem melpaDir melpaCommit stable workDir name recipe
  = Concurrently $ bracket (waitQSem sem) (\_ -> signalQSem sem) $ \_ -> do
    let packageBuildEl = melpaDir </> "package-build.el"
        recipeFile = melpaDir </> "recipes" </> T.unpack name
        sourceDir = workDir </> T.unpack name
    result <- runExceptT $ do
      recipeHash <- withExceptT HashRecipePackageError
                    (Nix.hash recipeFile)
      version <- withExceptT VersionPackageError
                 (getVersion packageBuildEl stable recipeFile name sourceDir)
      fetch0 <- withExceptT RevisionPackageError
                (case recipe of
                   Bzr {..} -> do
                     rev <- revision_Bzr sourceDir
                     return Nix.Bzr { Nix.url = url
                                    , Nix.rev = rev
                                    , Nix.sha256 = Nothing
                                    }
                   Git {..} -> do
                     rev <- revision_Git branch sourceDir
                     return Nix.Git { Nix.url = url
                                    , Nix.rev = rev
                                    , Nix.branchName = branch
                                    , Nix.sha256 = Nothing
                                    }
                   GitHub {..} -> do
                     rev <- revision_Git branch sourceDir
                     let url = "git://github.com/" <> repo <> ".git"
                     return Nix.Git { Nix.url = url
                                    , Nix.rev = rev
                                    , Nix.branchName = branch
                                    , Nix.sha256 = Nothing
                                    }
                   GitLab {..} -> do
                     rev <- revision_Git branch sourceDir
                     let url = "https://gitlab.com/" <> repo <> ".git"
                     return Nix.Git { Nix.url = url
                                    , Nix.rev = rev
                                    , Nix.branchName = branch
                                    , Nix.sha256 = Nothing
                                    }
                   CVS {..} -> do
                     return Nix.CVS { Nix.url = url
                                    , Nix.cvsModule = cvsModule
                                    , Nix.sha256 = Nothing
                                    }
                   Darcs {..} -> throwE (NotImplementedRevisionError "darcs")
                   Fossil {..} -> throwE (NotImplementedRevisionError "fossil")
                   Hg {..} -> do
                     rev <- revision_Hg sourceDir
                     return Nix.Hg { Nix.url = url
                                   , Nix.rev = rev
                                   , Nix.sha256 = Nothing
                                   }
                   SVN {..} -> do
                     rev <- revision_SVN sourceDir
                     return Nix.SVN { Nix.url = url
                                    , Nix.rev = rev
                                    , Nix.sha256 = Nothing
                                    }
                   Wiki {..} -> do
                     let url = fromMaybe defaultUrl wikiUrl
                         defaultUrl = "http://www.emacswiki.org/emacs/download/" <> name <> ".el"
                     return Nix.URL { Nix.url = url
                                    , Nix.sha256 = Nothing
                                    })
      (path, fetch) <- withExceptT FetchPackageError
                       (Nix.prefetch name fetch0)
      deps <- withExceptT DepsPackageError
              (M.keys <$> getDeps packageBuildEl recipeFile name path)
      return Nix.Package { Nix.version = version
                         , Nix.fetch = fetch
                         , Nix.deps = deps
                         , Nix.recipe = Nix.Recipe
                                        { Recipe.commit = melpaCommit
                                        , Recipe.sha256 = recipeHash
                                        }
                         }
    case result of
      Left err -> do
        hPutStrLn stderr (T.unpack name ++ ": " ++ show err)
        return Nothing
      Right pkg -> return (Just pkg)

data VersionError = NoVersion
                  | EmacsVersionError Int Text
                  | OtherVersionError SomeException
  deriving (Show)

getVersion :: FilePath -> Bool -> FilePath -> Text -> FilePath
           -> ExceptT VersionError IO Text
getVersion packageBuildEl stable recipeFile packageName sourceDir = do
  checkoutEl <- liftIO (getDataFileName "checkout.el")
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", checkoutEl
             , "-f", if stable then "checkout-stable" else "checkout"
             , recipeFile, T.unpack packageName, sourceDir
             ]
  runInteractiveProcess "emacs" args Nothing Nothing OtherVersionError EmacsVersionError
    $ \out -> do
      result <- liftIO (S.fold (<>) Nothing =<< S.map Just =<< S.decodeUtf8 out)
      case result of
        Nothing -> throwE NoVersion
        Just ver -> pure ver

data DepsError = ParseDepsError String
               | EmacsDepsError Int Text
               | OtherDepsError SomeException
  deriving (Show)

getDeps :: FilePath -> FilePath -> Text -> FilePath -> ExceptT DepsError IO (Map Text [Integer])
getDeps packageBuildEl recipeFile packageName sourceDirOrEl = do
  getDepsEl <- liftIO (getDataFileName "get-deps.el")
  isEl <- liftIO (doesFileExist sourceDirOrEl)
  let withSourceDir act
        | isEl = ExceptT $ do
            let tmpl = "melpa2nix-" <> T.unpack packageName
                elFile = T.unpack packageName <.> "el"
            withSystemTempDirectory tmpl $ \sourceDir -> do
              copyFile sourceDirOrEl (sourceDir </> elFile)
              runExceptT (act sourceDir)
        | otherwise = act sourceDirOrEl
  withSourceDir $ \sourceDir -> do
    let args = [ "--batch"
               , "-l", packageBuildEl
               , "-l", getDepsEl
               , "-f", "get-deps", recipeFile, T.unpack packageName, sourceDir
               ]
    runInteractiveProcess "emacs" args Nothing Nothing OtherDepsError EmacsDepsError
      $ \out -> do
        result <- liftIO (parseEither parseJSON <$> S.parseFromStream json' out)
        case result of
          Left err -> throwE (ParseDepsError err)
          Right deps_ -> pure deps_

data RevisionError = ParseRevisionError
                   | RevisionError Int Text
                   | NotImplementedRevisionError Text
                   | OtherRevisionError SomeException
  deriving (Show)

revision_Bzr :: FilePath -> ExceptT RevisionError IO Text
revision_Bzr tmp = do
  let args = [ "log", "-l1", tmp ]
  runInteractiveProcess "bzr" args Nothing Nothing OtherRevisionError RevisionError
    $ \out -> do
      let getRevno = (T.strip <$>) . T.stripPrefix "revno:"
      revnos <- mapMaybe getRevno <$> liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
      case revnos of
        (rev:_) -> pure rev
        _ -> throwE ParseRevisionError

revision_Git :: Maybe Text -> FilePath -> ExceptT RevisionError IO Text
revision_Git branch tmp
  = runInteractiveProcess "git" gitArgs (Just tmp) Nothing OtherRevisionError RevisionError
    $ \out -> do
      revs <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
      case revs of
        (rev:_) -> pure rev
        _ -> throwE ParseRevisionError
  where
    fullBranch = do
        branch_ <- branch
        -- package-build does not fetch all branches by default, so they must be referred
        -- to under the origin/ prefix
        return (T.unpack ("origin/" <> branch_))
    gitArgs = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
              ++ maybeToList fullBranch

revision_Hg :: FilePath -> ExceptT RevisionError IO Text
revision_Hg tmp
  = runInteractiveProcess "hg" ["tags"] (Just tmp) Nothing OtherRevisionError RevisionError
    $ \out -> do
      lines_ <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
      let revs = catMaybes (hgRev <$> lines_)
      case revs of
        (rev:_) -> pure rev
        _ -> throwE ParseRevisionError
  where
    hgRev txt = do
        afterTip <- T.strip <$> T.stripPrefix "tip" txt
        let (_, T.strip . T.takeWhile isHexDigit . T.drop 1 -> rev) = T.breakOn ":" afterTip
        if T.null rev
          then Nothing
          else return rev

revision_SVN :: FilePath -> ExceptT RevisionError IO Text
revision_SVN tmp
  = runInteractiveProcess "svn" ["info"] (Just tmp) Nothing OtherRevisionError RevisionError
    $ \out -> do
      lines_ <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
      let revs = catMaybes (svnRev <$> lines_)
      case revs of
        (rev:_) -> pure rev
        _ -> throwE ParseRevisionError
  where
    svnRev = fmap T.strip . T.stripPrefix "Revision:"
