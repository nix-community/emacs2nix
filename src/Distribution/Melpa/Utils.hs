module Distribution.Melpa.Utils where

import Control.Arrow ((***))
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Process
    ( CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess )

import Paths_melpa2nix (getDataFileName)

runScript :: FilePath -> HashMap Text Text -> IO (HashMap Text Text)
runScript _script _env = do
  _script <- getDataFileName _script
  _env <- return $ map (T.unpack *** T.unpack) (HM.toList _env)
  _env <- (_env ++) <$> getEnvironment
  let process = (proc _script [])
                { env = Just _env
                , std_out = CreatePipe
                }
  (Nothing, Just hOut, Nothing, pid) <- createProcess process
  res <- fromMaybe HM.empty . decode <$> B.hGetContents hOut
  _ <- waitForProcess pid
  return res
