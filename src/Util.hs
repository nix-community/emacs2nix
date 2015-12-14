{-# LANGUAGE DeriveDataTypeable #-}

module Util where

import Control.Concurrent.Async (Concurrently(..))
import Control.Exception
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S

data ProcessFailed
  = ProcessFailed String [String] (Maybe FilePath) (Maybe [(String, String)]) Int Text
  deriving (Show, Typeable)

instance Exception ProcessFailed

runInteractiveProcess
  :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)]
  -> (InputStream ByteString -> IO a)
  -> IO a
runInteractiveProcess cmd args cwd env withOutput = do
  (_, out, err, pid) <- S.runInteractiveProcess cmd args cwd env
  let
    getOutput = Concurrently (withOutput out)
    getErrors = Concurrently (S.fold (<>) T.empty =<< S.decodeUtf8 err)
    wait = Concurrently (S.waitForProcess pid)
  (result, errorMessage, exit) <- runConcurrently
                                  ((,,) <$> getOutput <*> getErrors <*> wait)
  case exit of
    ExitSuccess -> pure result
    ExitFailure code -> throwIO (ProcessFailed cmd args cwd env code errorMessage)

showExceptions :: IO b -> IO (Maybe b)
showExceptions go = catch (Just <$> go) handler
  where
    handler (SomeException e) = do
      out <- S.encodeUtf8 =<< S.unlines S.stdout
      S.write (Just (T.pack (show e))) out
      S.write Nothing out
      pure Nothing

showExceptions_ :: IO b -> IO ()
showExceptions_ go = showExceptions go >> pure ()
