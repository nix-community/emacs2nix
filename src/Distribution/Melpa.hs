{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa (getMelpa, readMelpa) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.QSem
import Control.Error hiding (err)
import Control.Exception (SomeException(..), bracket, handle)
import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.IO.Class
import Data.Aeson (parseJSON)
import Data.Aeson.Parser (json')
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Char (isHexDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempDirectory)

import Distribution.Melpa.Recipe
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Package as Nix

import Paths_emacs2nix (getDataFileName)

readMelpa :: FilePath -> IO (Maybe (Map Text Nix.Package))
readMelpa packagesJson =
  handle
  (\(SomeException _) -> return Nothing)
  (S.withFileAsInput packagesJson $ \inp ->
       parseMaybe parseJSON <$> S.parseFromStream json' inp)

getMelpa :: Int -> FilePath -> FilePath -> IO (Map Text Nix.Package)
getMelpa nthreads melpaDir workDir = do
  recipes <- readRecipes melpaDir

  createDirectoryIfMissing True workDir

  sem <- (if nthreads > 0 then pure nthreads else getNumCapabilities) >>= newQSem
  M.fromList . mapMaybe liftMaybe . M.toList
    <$> runConcurrently (M.traverseWithKey (getPackage sem melpaDir workDir) recipes)
  where
    liftMaybe (x, y) = (,) x <$> y

getPackage :: QSem -> FilePath -> FilePath -> Text -> Recipe -> Concurrently (Maybe Nix.Package)
getPackage sem melpaDir workDir name recipe
  = Concurrently $ bracket (waitQSem sem) (\_ -> signalQSem sem) $ \_ -> do
    putStrLn (T.unpack name)
    let packageBuildEl = melpaDir </> "package-build.el"
        recipeFile = melpaDir </> "recipes" </> T.unpack name
        sourceDir = workDir </> T.unpack name
    recipeExp <- S.withFileAsInput recipeFile (\inp -> S.fold (<>) T.empty =<< S.decodeUtf8 inp)
    runMaybeT $ do
      version <- getVersion packageBuildEl recipeFile name sourceDir
      fetch0 <- case recipe of
                  Bzr {..} -> do
                    rev <- revision_Bzr name sourceDir
                    return Nix.Bzr { Nix.url = url
                                   , Nix.rev = rev
                                   , Nix.sha256 = Nothing
                                   }
                  Git {..} -> do
                    rev <- revision_Git name branch sourceDir
                    return Nix.Git { Nix.url = url
                                   , Nix.rev = rev
                                   , Nix.branchName = branch
                                   , Nix.sha256 = Nothing
                                   }
                  GitHub {..} -> do
                    rev <- revision_Git name branch sourceDir
                    let url = "git://github.com/" <> repo <> ".git"
                    return Nix.Git { Nix.url = url
                                   , Nix.rev = rev
                                   , Nix.branchName = branch
                                   , Nix.sha256 = Nothing
                                   }
                  GitLab {..} -> do
                    rev <- revision_Git name branch sourceDir
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
                  Darcs {..} -> do
                    let errmsg = name <> ": fetcher 'darcs' not supported"
                    liftIO (S.write (Just errmsg) =<< S.encodeUtf8 S.stderr)
                    mzero
                  Fossil {..} -> do
                    let errmsg = name <> ": fetcher 'fossil' not supported"
                    liftIO (S.write (Just errmsg) =<< S.encodeUtf8 S.stderr)
                    mzero
                  Hg {..} -> do
                    rev <- revision_Hg name sourceDir
                    return Nix.Hg { Nix.url = url
                                  , Nix.rev = rev
                                  , Nix.sha256 = Nothing
                                  }
                  SVN {..} -> do
                    rev <- revision_SVN name sourceDir
                    return Nix.SVN { Nix.url = url
                                   , Nix.rev = rev
                                   , Nix.sha256 = Nothing
                                   }
                  Wiki {..} -> do
                    let url = fromMaybe defaultUrl wikiUrl
                        defaultUrl = "http://www.emacswiki.org/emacs/download/" <> name <> ".el"
                    return Nix.URL { Nix.url = url
                                   , Nix.sha256 = Nothing
                                   }
      (path, fetch) <- liftIO $ Nix.prefetch name fetch0
      deps <- M.keys <$> getDeps packageBuildEl recipeFile name path
      return Nix.Package { Nix.version = version
                         , Nix.fetch = fetch
                         , Nix.build = Nix.MelpaPackage
                                       { Nix.recipe = recipeExp
                                       , Nix.deps = deps
                                       }
                         }

getVersion :: FilePath -> FilePath -> Text -> FilePath -> MaybeT IO Text
getVersion packageBuildEl recipeFile packageName sourceDir = do
  checkoutEl <- liftIO (getDataFileName "checkout.el")
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", checkoutEl
             , "-f", "checkout", recipeFile, T.unpack packageName, sourceDir
             ]
  handleAll $ MaybeT (bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(_, out, _, _) -> S.fold (<>) Nothing =<< S.map Just =<< S.decodeUtf8 out))

getDeps :: FilePath -> FilePath -> Text -> FilePath -> MaybeT IO (Map Text [Integer])
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

revision_Bzr :: Text -> FilePath -> MaybeT IO Text
revision_Bzr name tmp = handleAll $ MaybeT $ do
  let args = [ "log", "-l1", tmp ]
  bracket
    (S.runInteractiveProcess "bzr" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(_, out, _, _) -> do
         let getRevno = (T.strip <$>) . T.stripPrefix "revno:"
         revnos <- liftM (mapMaybe getRevno)
                   $ S.lines out >>= S.decodeUtf8 >>= S.toList
         case revnos of
           (rev:_) -> return (Just rev)
           _ -> do
             let errmsg = name <> ": could not find revision"
             S.write (Just errmsg) =<< S.encodeUtf8 S.stderr
             return Nothing)

revision_Git :: Text -> Maybe Text -> FilePath -> MaybeT IO Text
revision_Git name branch tmp = MaybeT $ bracket
  (S.runInteractiveProcess "git" gitArgs (Just tmp) Nothing)
  (\(_, _, _, pid) -> S.waitForProcess pid)
  (\(_, out, _, _) -> do
       revs <- S.lines out >>= S.decodeUtf8 >>= S.toList
       case revs of
         (rev:_) -> return (Just rev)
         _ -> do
           let errmsg = name <> ": could not find revision"
           S.write (Just errmsg) =<< S.encodeUtf8 S.stderr
           return Nothing)
  where
    fullBranch = do
        branch_ <- branch
        -- package-build does not fetch all branches by default, so they must be referred
        -- to under the origin/ prefix
        return (T.unpack ("origin/" <> branch_))
    gitArgs = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
              ++ maybeToList fullBranch

revision_Hg :: Text -> FilePath -> MaybeT IO Text
revision_Hg name tmp = handleAll $ MaybeT $ bracket
  (S.runInteractiveProcess "hg" ["tags"] (Just tmp) Nothing)
  (\(_, _, _, pid) -> S.waitForProcess pid)
  (\(_, out, _, _) -> do
       lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
       let revs = catMaybes (hgRev <$> lines_)
       case revs of
         (rev:_) -> return (Just rev)
         _ -> do
           let errmsg = name <> ": could not find revision"
           S.write (Just errmsg) =<< S.encodeUtf8 S.stderr
           return Nothing)
  where
    hgRev txt = do
        afterTip <- T.strip <$> T.stripPrefix "tip" txt
        let (_, T.strip . T.takeWhile isHexDigit . T.drop 1 -> rev) = T.breakOn ":" afterTip
        if T.null rev
          then Nothing
          else return rev

revision_SVN :: Text -> FilePath -> MaybeT IO Text
revision_SVN name tmp = handleAll $ MaybeT $ bracket
  (S.runInteractiveProcess "svn" ["info"] (Just tmp) Nothing)
  (\(_, _, _, pid) -> S.waitForProcess pid)
  (\(_, out, _, _) -> do
       lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
       let revs = catMaybes (svnRev <$> lines_)
       case revs of
         (rev:_) -> return (Just rev)
         _ -> do
           let errmsg = name <> ": could not find revision"
           S.write (Just errmsg) =<< S.encodeUtf8 S.stderr
           return Nothing)
  where
    svnRev = fmap T.strip . T.stripPrefix "Revision:"

handleAll :: MaybeT IO a -> MaybeT IO a
handleAll act =
  MaybeT $ handle
    (\(SomeException e) -> do
         S.write (Just $ T.pack $ show e) =<< S.encodeUtf8 S.stderr
         return Nothing)
    (runMaybeT act)
