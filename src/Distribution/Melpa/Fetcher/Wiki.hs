{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Wiki where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Process (readProcess)

import Distribution.Melpa.Version

data Wiki =
  Fetcher
  { url :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Wiki where
  toJSON = genericToJSON defaultOptions

instance FromJSON Wiki where
  parseJSON = genericParseJSON defaultOptions

stabilize :: FilePath -> Text -> Version -> Wiki -> Wiki
stabilize _ _ _ = id

hash :: FilePath -> Text -> Wiki -> IO Text
hash _ name Fetcher {..} = do
  let defaultUrl = "http://www.emacswiki.org/emacs/download/"
                   ++ T.unpack name ++ ".el"
      url_ = maybe defaultUrl T.unpack url
  hash_ <- readProcess "nix-prefetch-url" [url_] ""
  return (T.pack hash_)
