{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Utils (getCommit, prefetch) where

import Control.Arrow ((***))
import Control.Error hiding (err, runScript)
import Control.Exception (bracket, handle)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Paths_melpa2nix (getDataFileName)

runScript :: FilePath -> Maybe FilePath -> HashMap Text Text
          -> EitherT Text IO (HashMap Text Text)
runScript script workDir env =
  EitherT $ handle
    (\(S.ParseException _) -> return $ Left $ T.pack script <> ": unable to parse response")
    (do
         scriptPath <- getDataFileName script
         env' <- Just <$> withEnv env
         bracket
           (S.runInteractiveProcess scriptPath [] workDir env')
           (\(_, _, _, pid) -> S.waitForProcess pid)
           (\(inp, out, err, _) -> do
                  S.write Nothing inp
                  S.supply err S.stderr
                  val <- S.parseFromStream Aeson.json' out
                  return $ case Aeson.parseEither Aeson.parseJSON val of
                    Left msg -> Left $ T.pack script <> ": " <> T.pack msg
                    Right hm -> Right hm))

withEnv :: HashMap Text Text -> IO [(String, String)]
withEnv env' =
  unpackTuples . HM.toList . (<> env') . HM.fromList . packTuples <$> getEnvironment
  where
    unpackTuples = map (T.unpack *** T.unpack)
    packTuples = map (T.pack *** T.pack)

getCommit :: FilePath -> Bool -> Text -> Maybe FilePath -> HashMap Text Text
          -> EitherT Text IO Text
getCommit _melpa stable name workDir envIn = do
  let envIn' = HM.singleton "melpa" (T.pack _melpa) <> envIn
  response <- runScript script workDir envIn'
  HM.lookup name response ?? (name <> ": no commit")
  where
    script | stable = "get-stable-commit.sh"
           | otherwise = "get-commit.sh"

prefetch :: FilePath -> Text -> Maybe FilePath -> HashMap Text Text -> EitherT Text IO Text
prefetch _nixpkgs name workDir envIn = do
  let envIn' = HM.singleton "nixpkgs" (T.pack _nixpkgs) <> envIn
  response <- runScript "prefetch.sh" workDir envIn'
  HM.lookup name response ?? (name <> ": no hash")
