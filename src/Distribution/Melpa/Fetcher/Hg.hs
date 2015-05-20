{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Melpa.Fetcher.Hg ( Hg, fetchHg ) where

import Control.Applicative
import Control.Error
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Attoparsec.ByteString.Char8
import Data.Char (isHexDigit)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Prelude hiding (takeWhile)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Fetcher

data Hg =
  Hg
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Hg where
  toJSON = wrapFetcher "hg" . genericToJSON defaultOptions

instance FromJSON Hg where
  parseJSON = genericParseJSON defaultOptions

fetchHg :: Fetcher Hg
fetchHg = Fetcher {..}
  where
    getRev _ Hg {..} tmp =
      handleAll $ EitherT $ bracket
        (S.runInteractiveProcess "hg" ["tags"] (Just tmp) Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
               let revs = catMaybes (hgRev <$> lines_)
               return $ headErr ("could not find revision in:\n" <> T.unlines lines_) revs)

    prefetch name Hg {..} rev =
      prefetchWith name "nix-prefetch-hg" [ T.unpack url, T.unpack rev ]

hgRev :: Text -> Maybe Text
hgRev txt = do
    afterTip <- T.strip <$> T.stripPrefix "tip" txt
    let (_, T.strip . T.takeWhile isHexDigit . T.drop 1-> rev) = T.breakOn ":" afterTip
    if T.null rev
      then Nothing
      else return rev
