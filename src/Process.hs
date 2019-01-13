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

module Process where

import Control.Concurrent.Async (Concurrently(..))
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import System.Exit (ExitCode(..))
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S
import qualified Data.Text.Prettyprint.Doc as Pretty

import Exceptions


slurp :: InputStream ByteString -> IO Text
slurp stream = S.fold (<>) T.empty =<< S.decodeUtf8 stream

runInteractiveProcess
  :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)]
  -> (InputStream ByteString -> IO a)
  -> IO a
runInteractiveProcess cmd args cwd env withOutput =
  inContext ("process " <> Pretty.pretty (S.showCommandForUser cmd args)) $ do
    (_, out, err, pid) <- S.runInteractiveProcess cmd args cwd env
    let
      getOutput =
        Concurrently
         (catch
          (fmap Right $ withOutput out)
          (\e -> (fmap (Left . ((,) e)) (slurp out))))
          ---- Maybe catch exceptions that might occur by just slurping the input stream
          ---- couldn't get this to type (Ambigous exception type)
          -- (\e -> catch
          --        (fmap (Left . ((,) e)) (slurp out))
          --        -- (Left (e, slurp out))
          --        (\ei -> pure $ Left (e, T.pack $ "INPUT ERROR " ++ show ei))))
      getErrors = Concurrently $ slurp err
      wait = Concurrently (S.waitForProcess pid)
    (output, errorMessage, exit) <- runConcurrently ((,,) <$> getOutput <*> getErrors <*> wait)
    case (exit, output) of
      (ExitSuccess, Right v) -> pure v
      (ExitSuccess, Left (ex, leftover)) -> throwM $ ProcessingFailed leftover errorMessage ex
      (ExitFailure code, Left info) -> throwM $ trace (show info) $ Died code errorMessage
      (ExitFailure code, Right _) ->
          throwM $ Died code "the improbable happened: successfully parsed \
                             \output from failed process"
