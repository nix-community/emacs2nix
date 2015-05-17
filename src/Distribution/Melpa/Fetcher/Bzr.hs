{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Bzr ( Bzr, fetchBzr ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Error
import Control.Exception (bracket)
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified System.IO.Streams as S

import Distribution.Melpa.Fetcher

data Bzr =
  Bzr
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Bzr where
  toJSON = wrapFetcher "bzr" . genericToJSON defaultOptions

instance FromJSON Bzr where
  parseJSON = genericParseJSON defaultOptions

fetchBzr :: Fetcher Bzr
fetchBzr = Fetcher {..}
  where
    getRev :: Text -> Bzr -> FilePath -> EitherT Text IO Text
    getRev name Bzr {..} tmp =
      let args = [ "log", "-l1", tmp ]
      in EitherT $ bracket
        (S.runInteractiveProcess "bzr" args Nothing Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               let getRevno = (T.strip <$>) . T.stripPrefix "revno:"
               revnos <- liftM (mapMaybe getRevno)
                         $ S.lines out >>= S.decodeUtf8 >>= S.toList
               return $ headErr (name <> ": could not find revision") revnos)

    prefetch :: Text -> f -> Text -> EitherT Text IO (FilePath, Text)
    prefetch name Bzr {..} rev =
      let args = []
      in EitherT $ bracket
        (S.runInteractiveProcess "bzr" args Nothing Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               let getHash = T.stripPrefix "hash is "
                   getPath = T.stripPrefix "path is "
               hashes <- liftM (mapMaybe getHash) $ S.lines out >>= S.decodeUtf8 >>= S.toList
               paths <- liftM (mapMaybe getPath) $ S.lines out >>= S.decodeUtf8 >>= S.toList
               hash <- return $ headErr (name <> ": could not find hash") hashes
               path <- return $ headErr (name <> ": could not find path") paths
               return (path, hash))
