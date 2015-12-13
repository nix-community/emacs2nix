module Util (runInteractiveProcess) where

import Control.Error hiding (err)
import Control.Exception (SomeException(..), handle)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S

runInteractiveProcess
  :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)]
  -> (SomeException -> e) -> (Int -> Text -> e)
  -> (InputStream ByteString -> ExceptT e IO a)
  -> ExceptT e IO a
runInteractiveProcess cmd args cwd env withExc withExit withOutput
  = ExceptT $ handle
    (pure . Left . withExc)
    (do (_, out, err, pid) <- S.runInteractiveProcess cmd args cwd env
        result <- runExceptT (withOutput out)
        errmsg <- S.fold (<>) T.empty =<< S.decodeUtf8 err
        exit <- S.waitForProcess pid
        case exit of
          ExitSuccess -> pure result
          ExitFailure code -> pure (Left (withExit code errmsg)))
