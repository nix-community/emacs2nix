{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Archive where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (defaultOptions, parseEither)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics
import Network.Http.Client (get)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Distribution.Melpa.Version

data Archive =
  Archive
  { ver :: Version
  , deps :: Maybe (HashMap Text Version)
  }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Archive where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Archive where
  toJSON = genericToJSON defaultOptions

fetchArchive :: IO (HashMap Text Archive)
fetchArchive =
  get "http://melpa.org/archive.json" $ \_ inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result

readArchive :: FilePath -> IO (HashMap Text Archive)
readArchive path =
  S.withFileAsInput path $ \inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result
