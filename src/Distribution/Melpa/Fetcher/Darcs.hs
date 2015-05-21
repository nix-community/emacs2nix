{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Darcs ( Darcs, fetchDarcs ) where

import Control.Error
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import qualified System.IO.Streams as S
import System.IO.Temp (withSystemTempFile)

import Distribution.Melpa.Fetcher

data Darcs =
  Darcs
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Darcs where
  toJSON = wrapFetcher "darcs" . genericToJSON defaultOptions

instance FromJSON Darcs where
  parseJSON = genericParseJSON defaultOptions

fetchDarcs :: Fetcher Darcs
fetchDarcs = Fetcher {..}
  where
    getRev _ _ _ = left "fetcher 'darcs' unsupported"
    {-
    getRev _ Darcs {..} tmp =
      handleAll $ liftIO $ bracket
        (S.runInteractiveProcess "darcs" args (Just tmp) Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               S.decodeUtf8 out >>= S.fold (<>) T.empty)
      where args = [ "changes", "--context" ]
    -}

    prefetch _ _ _ = left "fetcher 'darcs' unsupported"
    {-
    prefetch name Darcs {..} context =
      handleAll $ EitherT $ withSystemTempFile "melpa2nix-darcs-context"
        $ \contextFile handle -> do
               -- darcs uses a long textual context instead of revision
               -- numbers or hashes, so we need to put it in a file
               T.hPutStr handle context
               let args = [ T.unpack url, contextFile ]
               runEitherT $ prefetchWith name "nix-prefetch-darcs" args
    -}
