{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Paths_melpa2nix (getDataFileName)
import System.Environment (getEnvironment)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess )

import Distribution.Melpa.Archive
import Distribution.Melpa.Fetcher
import qualified Distribution.Melpa.Fetcher.Bzr as Bzr
import qualified Distribution.Melpa.Fetcher.CVS as CVS
import qualified Distribution.Melpa.Fetcher.Darcs as Darcs
import qualified Distribution.Melpa.Fetcher.Fossil as Fossil
import qualified Distribution.Melpa.Fetcher.Git as Git
import qualified Distribution.Melpa.Fetcher.GitHub as GitHub
import qualified Distribution.Melpa.Fetcher.Hg as Hg
import qualified Distribution.Melpa.Fetcher.SVN as SVN
import qualified Distribution.Melpa.Fetcher.Wiki as Wiki
import Distribution.Melpa.Recipe
import Distribution.Melpa.Version

data Package =
  Package
  { ver :: Version
  , deps :: HashMap Text Version
  , recipe :: Recipe
  , hash :: Text
  }
  deriving (Eq, Generic)

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions

hashPackage :: FilePath -> FilePath -> Text -> Archive -> Recipe
            -> IO (Maybe Package)
hashPackage melpa nixpkgs name Archive {..} recipe_ = runMaybeT $ do
  commit <- getCommit melpa name recipe_
  let recipe = addCommit commit recipe_
  hash <- prefetch melpa name recipe
  return Package {..}

hashPackageStable :: FilePath -> FilePath -> Text -> Archive -> Recipe
                  -> IO (Maybe Package)
hashPackageStable melpa nixpkgs name Archive {..} recipe_ = runMaybeT $ do
  commit <- getStableCommit melpa name recipe_
  let recipe = addCommit commit recipe_
  hash <- prefetch melpa name recipe
  return Package {..}

getCommit :: FilePath -> Text -> Text -> MaybeT IO Text
getCommit melpa name fetcher = MaybeT $ do
  getCommitSh <- getDataFileName "get-commit.sh"
  inheritEnv <- getEnvironment
  let env' = inheritEnv
             ++
             [ ("melpa", T.unpack melpa)
             , ("name", T.unpack name)
             , ("fetcher", T.unpack fetcher)
             ]
      process = (proc getCommitSh [])
                { env = Just env'
                , std_out = CreatePipe
                }
  (Nothing, Just hOut, Nothing, pid) <- createProcess process
  txt <- B.hGetContents hOut
  waitForProcess pid
  let hm = decode txt
  return (HM.lookup name)

getStableCommit :: FilePath -> Text -> Recipe -> MaybeT IO Text
getStableCommit melpa name fetcher = MaybeT $ do
  getCommitSh <- getDataFileName "get-stable-commit.sh"
  inheritEnv <- getEnvironment
  let env' = inheritEnv
             ++
             [ ("melpa", T.unpack melpa)
             , ("name", T.unpack name)
             , ("fetcher", T.unpack fetcher)
             ]
      process = (proc getCommitSh [])
                { env = Just env'
                , std_out = CreatePipe
                }
  (Nothing, Just hOut, Nothing, pid) <- createProcess process
  txt <- B.hGetContents hOut
  waitForProcess pid
  let hm = decode txt
  return (HM.lookup name)

prefetch :: FilePath -> Text -> Recipe -> MaybeT IO Text
