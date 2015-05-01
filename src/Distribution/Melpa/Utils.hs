{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Utils (getCommit, prefetch) where

import Control.Arrow ((***))
import Control.Error hiding (runScript)
import Control.Monad.IO.Class
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (getEnvironment)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess )

import Paths_melpa2nix (getDataFileName)

runScript :: FilePath -> HashMap Text Text -> EitherT Text IO (HashMap Text Text)
runScript script (unpackEnv -> envIn) = do
  scriptPath <- liftIO $ getDataFileName script
  envMerged <- (++ envIn) <$> liftIO getEnvironment
  let process = (proc scriptPath [])
                { env = Just envMerged
                , std_out = CreatePipe
                }
  (Nothing, Just hOut, Nothing, pid) <- liftIO $ createProcess process
  response <- liftIO $ B.hGetContents hOut
  let result = decode response
  _ <- liftIO $ waitForProcess pid
  case result of
    Nothing ->
      left (T.pack script <> ": could not parse:\n" <> T.decodeUtf8 (B.toStrict response))
    Just hm -> return hm

unpackEnv :: HashMap Text Text -> [(String, String)]
unpackEnv = map (T.unpack *** T.unpack) . HM.toList

getCommit :: FilePath -> Bool -> Text -> HashMap Text Text -> EitherT Text IO Text
getCommit melpa stable name envIn = do
  let envIn' = HM.singleton "melpa" (T.pack melpa) <> envIn
  response <- runScript script envIn'
  HM.lookup name response ?? (name <> ": no commit")
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> HashMap Text Text -> EitherT Text IO Text
prefetch nixpkgs name envIn = do
  let envIn' = HM.singleton "nixpkgs" (T.pack nixpkgs) <> envIn
  response <- runScript "prefetch.sh" envIn'
  HM.lookup name response ?? (name <> ": no hash")
