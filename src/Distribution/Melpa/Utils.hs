{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Utils (getCommit, prefetch) where

import Control.Error hiding (runScript)
import Control.Exception (bracket)
import qualified Control.Foldl as F
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Filesystem.Path.CurrentOS as Path
import Prelude hiding (FilePath)
import Turtle

import Paths_melpa2nix (getDataFileName)

runScript :: FilePath -> HashMap Text Text -> EitherT Text IO (HashMap Text Text)
runScript script args = do
  response <- liftIO $ do
    scriptPath <- T.pack <$> getDataFileName (Path.encodeString script)
    withEnv args
      (B.fromChunks . map T.encodeUtf8
       <$> fold (inproc scriptPath [] empty) F.list)
  let result = Aeson.decode response
  case result of
    Nothing -> do
      scriptForUser <- hoistEither (toText script)
      left (scriptForUser <> ": could not parse:\n"
            <> T.decodeUtf8 (B.toStrict response))
    Just hm -> return hm

withEnv :: HashMap Text Text -> IO a -> IO a
withEnv env_ act = bracket setupEnv restoreEnv (\_ -> act)
  where
    replaceEnv var val = do
      old <- need var
      export var val
      return old
    unreplaceEnv var mval =
      case mval of
        Nothing -> unset var
        Just val -> export var val
    setupEnv = HM.traverseWithKey replaceEnv env_
    restoreEnv = HM.traverseWithKey unreplaceEnv

getCommit :: FilePath -> Bool -> Text -> HashMap Text Text -> EitherT Text IO Text
getCommit _melpa stable name envIn = do
  _melpa <- hoistEither (toText _melpa)
  let envIn' = HM.singleton "melpa" _melpa <> envIn
  response <- runScript script envIn'
  HM.lookup name response ?? (name <> ": no commit")
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> HashMap Text Text -> EitherT Text IO Text
prefetch _nixpkgs name envIn = do
  _nixpkgs <- hoistEither (toText _nixpkgs)
  let envIn' = HM.singleton "nixpkgs" _nixpkgs <> envIn
  response <- runScript "prefetch.sh" envIn'
  HM.lookup name response ?? (name <> ": no hash")
