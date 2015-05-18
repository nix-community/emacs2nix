{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Git ( Git, fetchGit, getRev_Git ) where

import Control.Error hiding (runScript)
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified System.IO.Streams as S

import Distribution.Melpa.Fetcher

data Git =
  Git
  { url :: Text
  , commit :: Maybe Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Git where
  toJSON = wrapFetcher "git" . genericToJSON defaultOptions

instance FromJSON Git where
  parseJSON = genericParseJSON defaultOptions

fetchGit :: Fetcher Git
fetchGit = Fetcher {..}
  where
    getRev name Git {..} tmp = getRev_Git name branch tmp
    prefetch name Git {..} rev =
      let args = [ "--url", T.unpack url, "--rev", T.unpack rev ]
      in prefetchWith name "nix-prefetch-git" args

getRev_Git :: Text -> Maybe Text -> FilePath -> EitherT Text IO Text
getRev_Git name branch tmp =
  EitherT $ bracket
    (S.runInteractiveProcess "git" gitArgs (Just tmp) Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           revs <- S.lines out >>= S.decodeUtf8 >>= S.toList
           return $ headErr (name <> ": could not find revision") revs)
  where
    gitArgs = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
              ++ maybeToList (T.unpack <$> branch)
