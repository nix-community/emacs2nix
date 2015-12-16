{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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

data Died = Died Int Text
  deriving (Show, Typeable)

instance Exception Died

data ProcessFailed = ProcessFailed String [String] SomeException
  deriving (Show, Typeable)

instance Exception ProcessFailed

runInteractiveProcess
  :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)]
  -> (InputStream ByteString -> IO a)
  -> IO a
runInteractiveProcess cmd args cwd env withOutput
  = mapExceptionIO (ProcessFailed cmd args) $ do
    (_, out, err, pid) <- S.runInteractiveProcess cmd args cwd env
    let
      getOutput = Concurrently (withOutput out)
      getErrors = Concurrently (S.fold (<>) T.empty =<< S.decodeUtf8 err)
      wait = Concurrently (S.waitForProcess pid)
    (result, errorMessage, exit) <- runConcurrently
                                    ((,,) <$> getOutput <*> getErrors <*> wait)
    case exit of
      ExitSuccess -> pure result
      ExitFailure code -> throwIO (Died code errorMessage)

showExceptions :: IO b -> IO (Maybe b)
showExceptions go = catch (Just <$> go) handler
  where
    handler (SomeException e) = do
      S.write (Just (T.pack (show e))) =<< S.encodeUtf8 =<< S.unlines S.stdout
      pure Nothing

showExceptions_ :: IO b -> IO ()
showExceptions_ go = showExceptions go >> pure ()

mapExceptionIO :: (Exception e, Exception f) => (e -> f) -> IO a -> IO a
mapExceptionIO f go = catch go handler where
  handler e = throwIO (f e)
