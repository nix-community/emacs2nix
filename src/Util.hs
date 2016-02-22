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
