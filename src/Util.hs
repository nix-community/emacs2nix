module Util (runInteractiveProcess) where

import Control.Error hiding (err)
import Control.Exception (SomeException(..), handle)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S

runInteractiveProcess :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)]
                      -> (InputStream ByteString -> EitherT String IO a)
                      -> EitherT String IO a
runInteractiveProcess cmd args cwd env withOutput
  = EitherT $ handle
    (\(SomeException e) -> return (Left (msg ++ "exception: " ++ show e)))
    (do (_, out, err, pid) <- S.runInteractiveProcess cmd args cwd env
        result <- runEitherT (withOutput out)
        errmsg <- S.fold (<>) T.empty =<< S.decodeUtf8 err
        exit <- S.waitForProcess pid
        case exit of
          ExitSuccess -> return result
          ExitFailure code -> return (Left (msg ++ "exit code " ++ show code ++ ":\n"
                                            ++ T.unpack errmsg)))
  where
    msg = "command `" ++ unwords (cmd : args) ++ "` failed with "
