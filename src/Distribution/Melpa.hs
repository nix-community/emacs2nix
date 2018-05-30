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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa ( updateMelpa ) where

import Control.Concurrent ( getNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Concurrent.QSem
import Control.Error hiding ( err )
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Char ( isDigit, isHexDigit )
import Data.Foldable ( toList )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as M
import Data.Monoid ( (<>) )
import Data.Scientific ( floatingOrInteger )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.Text.Read ( decimal )
import Data.Typeable ( Typeable )
import Data.Version ( Version (..), makeVersion )
import qualified Network.Http.Client as HTTP
import System.Directory ( createDirectoryIfMissing, copyFile )
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import Text.Taggy.Parser (taggyWith)
import Text.Taggy.Types ( Tag (..), Attribute(..) )

import qualified Distribution.Emacs.Name as Emacs
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
            -> FilePath  -- ^ map of Emacs names to Nix names
            -> Bool  -- ^ only generate the index
            -> Set Text
            -> IO ()
updateMelpa melpaDir stable workDir melpaOut namesMapFile indexOnly packages = do
  namesMap <- Nix.readNames namesMapFile

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
            (\_ -> getPackage melpa stable workDir namesMap pkg))
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


data PkgInfo =
  PkgInfo
  { version :: !Version
  , deps :: HashMap Text Version
  }


parseVersion :: Value -> Parser Version
parseVersion =
  withArray "version" $ fmap makeVersion . traverse parseNatural . toList
  where
    parseNatural =
      withScientific "natural number" $ \x ->
      case floatingOrInteger x of
        Left (r :: Double) -> fail ("not an integer: " ++ show r)
        Right i
          | i < 0     -> fail ("not non-negative: " ++ show i)
          | otherwise -> pure i


parsePkgInfo :: Value -> Parser PkgInfo
parsePkgInfo =
  withObject "pkg-info" $ \obj ->
  do
    version <- parseVersion =<< obj .: "ver"
    deps <- parseDeps =<< obj .: "deps"
    pure PkgInfo {..}
  where
    parseDeps =
      withObject "dependencies" $ traverse parseVersion


{-|

To create an expression for an Emacs package, we need several pieces of
information:

1. name
2. version
3. source
  a. the method to fetch the source
  b. the commit (or other token) to fetch the source reproducibly
  c. the hash of the source
4. dependencies
5. exact recipe

The name is available from the enumerated list of recipes or the arguments
on the command line. The version and source can be obtained after the package
is unpacked by 'package-build'. The dependencies are most easily determined
after the package is built by 'package-build'. It is not strictly necessary to
build every package just to write the Nix expression, but the build status can
be used to mark packages broken as appropriate. The exact recipe is available
at any time; we track this so that the Nix expression will reproduce the exact
build even if the recipe is changed upstream.

As elucidated above, there are two phases of the 'package-build' workflow which
we must execute: 'package-build-checkout' (to checkout the package) and
'package-build-package' (to build the checked-out source). 'package-build'
expects to be run from the MELPA tree, so in order to build each package in
isolation (and to keep the MELPA tree clean) we construct enough of a "fake"
tree that 'package-build' is happy. For our purposes, it suffices to create a
directory with one recipe (the target package) in the "recipes"
subdirectory. 'package-build' will create a "working" subdirectory with the
package source and a "packages" subdirectory with the build product.

 -}

getPackage :: Melpa -> Bool -> FilePath
           -> HashMap Emacs.Name Nix.Name
           -> (Text, Recipe)
           -> IO (Maybe Nix.Package)
getPackage melpa@(Melpa {..}) stable tmpDir namesMap (name, recipe) =
  showExceptions $ mapExceptionIO (PackageException name) $ do
    let
      recipeFile = recipeFileName melpa name

      packageDir = tmpDir </> T.unpack name
      recipesDir = packageDir </> "recipes"
      workingDir = packageDir </> "working"
      archiveDir = packageDir </> "packages"
      sourceDir = workingDir </> T.unpack name

    createDirectoryIfMissing True recipesDir
    createDirectoryIfMissing True workingDir
    createDirectoryIfMissing True archiveDir
    copyFile recipeFile (recipesDir </> T.unpack name)

    melpaRecipe <- freezeRecipe melpa name
    pkgInfo <- build melpa stable name packageDir
    (_, fetch) <- Nix.prefetch name =<< freezeSource name sourceDir recipe

    nixName <- Nix.getName namesMap (Emacs.Name name)
    nixDeps <- mapM (Nix.getName namesMap . Emacs.Name) (HashMap.keys $ deps pkgInfo)

    pure
      Nix.Package
      { Nix.pname = nixName
      , Nix.version = version pkgInfo
      , Nix.fetch = fetch
      , Nix.deps = nixDeps
      , Nix.recipe = melpaRecipe
      }

build :: Melpa -> Bool -> Text -> FilePath -> IO PkgInfo
build melpa stable name packageDir =
  do
    buildEl <- getDataFileName "build.el"
    let
      args =
        [ "-Q", "--batch"
        , "-L", packageBuildDir melpa
        , "-l", buildEl
        , "-f", if stable then "build-stable" else "build"
        , T.unpack name
        ]
      cwd = Just packageDir
    runInteractiveProcess "emacs" args cwd Nothing $ \out -> do
      r <- liftIO (parseEither parsePkgInfo <$> S.parseFromStream json' out)
      case r of
        Left err -> throwIO (ParsePkgInfoError err)
        Right pkgInfo -> pure pkgInfo

recipeFileName :: Melpa -> Text -> FilePath
recipeFileName Melpa {..} (T.unpack -> name) = melpaDir </> "recipes" </> name

freezeRecipe :: Melpa -> Text -> IO Nix.Recipe
freezeRecipe melpa@(Melpa {..}) name = do
  let recipe = recipeFileName melpa name
  hash <- Nix.hash recipe
  commit <-
    let
      args =
        [ "log"
        , "--first-parent"
        , "-n", "1"
        , "--pretty=format:%H"
        , "--", recipe
        ]
      getRecipeRevision out =
        do
          revs <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
          case revs of
            (rev:_) -> pure rev
            _ -> throwIO NoRevision
    in
      runInteractiveProcess "git" args (Just melpaDir) Nothing getRecipeRevision
  pure
    Nix.Recipe
    { Recipe.ename = name
    , Recipe.commit = commit
    , Recipe.sha256 = hash
    }

data DarcsFetcherNotImplemented = DarcsFetcherNotImplemented
  deriving (Show, Typeable)

instance Exception DarcsFetcherNotImplemented

data FossilFetcherNotImplemented = FossilFetcherNotImplemented
  deriving (Show, Typeable)

instance Exception FossilFetcherNotImplemented

freezeSource :: Text -> FilePath -> Recipe -> IO Nix.Fetch

freezeSource _ sourceDir Bzr {..} = do
  rev <- revision_Bzr sourceDir
  pure Nix.Bzr { Nix.url = url
               , Nix.rev = rev
               , Nix.sha256 = Nothing
               }

freezeSource _ sourceDir Git {..} = do
  rev <- revision_Git branch sourceDir
  pure Nix.Git { Nix.url = url
               , Nix.rev = rev
               , Nix.branchName = branch
               , Nix.sha256 = Nothing
               }

freezeSource _ sourceDir GitHub {..} = do
  rev <- revision_Git branch sourceDir
  let (owner:rest) = T.splitOn "/" repo
  pure Nix.GitHub { Nix.owner = owner
                  , Nix.repo = T.concat rest
                  , Nix.rev = rev
                  , Nix.sha256 = Nothing
                  }

freezeSource _ sourceDir GitLab {..} = do
  rev <- revision_Git branch sourceDir
  let (owner:rest) = T.splitOn "/" repo
  pure Nix.GitLab { Nix.owner = owner
                  , Nix.repo = T.concat rest
                  , Nix.rev = rev
                  , Nix.sha256 = Nothing
                  }

freezeSource _ _ CVS {..} =
  pure Nix.CVS { Nix.cvsRoot = url
               , Nix.cvsModule = cvsModule
               , Nix.sha256 = Nothing
               }

freezeSource _ sourceDir Hg {..} = do
  rev <- revision_Hg sourceDir
  pure Nix.Hg { Nix.url = url
              , Nix.rev = rev
              , Nix.sha256 = Nothing
              }

freezeSource _ sourceDir Bitbucket {..} = do
  let url = "https://bitbucket.com/" <> repo
  rev <- revision_Hg sourceDir
  pure Nix.Hg { Nix.url = url
              , Nix.rev = rev
              , Nix.sha256 = Nothing
              }

freezeSource _ sourceDir SVN {..} = do
  rev <- revision_SVN sourceDir
  pure Nix.SVN { Nix.url = url
               , Nix.rev = rev
               , Nix.sha256 = Nothing
               }

freezeSource name _ (w@Wiki {..}) = do
  guessedRevision <- guessWikiRevision name w
  let
    defaultUrl = "https://www.emacswiki.org/emacs/download/" <> name <> ".el"
    versionedDefaultUrl =
      case guessedRevision of
        Nothing -> defaultUrl
        Just version -> defaultUrl <> "?revision=" <> T.pack (show version)
    url = fromMaybe versionedDefaultUrl wikiUrl
  pure Nix.URL { Nix.url = url
               , Nix.sha256 = Nothing
               , Nix.name = Just (name <> ".el")
               }

freezeSource _ _ Darcs {..} = throwIO DarcsFetcherNotImplemented

freezeSource _ _ Fossil {..} = throwIO FossilFetcherNotImplemented

guessWikiRevision :: Text -> Recipe -> IO (Maybe Integer)
guessWikiRevision _ Wiki { wikiUrl = Just _ } =
  -- No guessing when there is an explicitly set wiki URL
  return $ Nothing

guessWikiRevision name Wiki {} = do
   body <- getAsText revisionsPageUrl
   -- TL.unpack body `trace` return ()
   return $ findLatestRevision $ taggyWith True body
  where
       revisionsPageUrl :: B.ByteString
       revisionsPageUrl = TE.encodeUtf8 $ "https://www.emacswiki.org/emacs?action=history;id=" <> name <> ".el"

       defaultPageUrl :: Text
       defaultPageUrl = "https://www.emacswiki.org/emacs/" <> name <> ".el"

       defaultRevisionAttr = Attribute "href" defaultPageUrl

       getAsText :: B.ByteString -> IO TL.Text
       getAsText url = TLE.decodeUtf8 . BL.fromStrict <$> HTTP.get url HTTP.concatHandler

       readDecimal :: Text -> Maybe Integer
       readDecimal aText = case decimal aText of
         Left _ -> Nothing
         Right (i, _) -> Just i

       revisionPrefix = "Revision "

       findLatestRevision [] = Nothing
       findLatestRevision (TagOpen "a" attrs _ : TagText aText : tags)
         | elem defaultRevisionAttr attrs && T.isPrefixOf revisionPrefix aText
           = readDecimal $ T.drop (T.length revisionPrefix) aText
         | otherwise = findLatestRevision tags
       findLatestRevision (_:tags) = findLatestRevision tags

guessWikiRevision _ _ = return Nothing

data NoVersion = NoVersion
  deriving (Show, Typeable)

instance Exception NoVersion

packageBuildDir :: Melpa -> FilePath
packageBuildDir Melpa {..} = melpaDir </> "package-build"

data ParsePkgInfoError = ParsePkgInfoError String
  deriving (Show, Typeable)

instance Exception ParsePkgInfoError

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
