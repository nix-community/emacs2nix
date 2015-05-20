{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Hg ( Hg, fetchHg ) where

import Control.Applicative
import Control.Error
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Attoparsec.ByteString.Char8
import Data.Char (isHexDigit)
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
               revs <- S.parseFromStream (many parseHgRev) out
               return $ headErr "could not find revision" $ catMaybes revs)

    prefetch name Hg {..} rev =
      prefetchWith name "nix-prefetch-hg" args
      where
        args = [ T.unpack url, T.unpack rev ]

parseHgRev :: Parser (Maybe Text)
parseHgRev = go <|> skipLine
  where
    skipLine = skipWhile (/= '\n') *> ((char '\n' *> pure ()) <|> endOfInput) *> pure Nothing
    go = do
      _ <- string "tip"
      skipSpace
      skipWhile isDigit
      _ <- char ':'
      rev <- takeWhile isHexDigit
      _ <- char '\n'
      return (Just $ T.decodeUtf8 rev)
