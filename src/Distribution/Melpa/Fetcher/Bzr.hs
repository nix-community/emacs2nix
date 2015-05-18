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
    getRev name Bzr {..} tmp =
      handleAll $ EitherT $ bracket
        (S.runInteractiveProcess "bzr" args Nothing Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               let getRevno = (T.strip <$>) . T.stripPrefix "revno:"
               revnos <- liftM (mapMaybe getRevno)
                         $ S.lines out >>= S.decodeUtf8 >>= S.toList
               return $ headErr "could not find revision" revnos)
      where args = [ "log", "-l1", tmp ]

    prefetch name Bzr {..} rev =
      handleAll
      $ prefetchWith name "nix-prefetch-bzr" [ T.unpack url, T.unpack rev ]
