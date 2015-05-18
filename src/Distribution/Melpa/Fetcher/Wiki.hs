{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Wiki ( Wiki, fetchWiki ) where

import Control.Error hiding (runScript)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Fetcher

data Wiki =
  Wiki
  { url :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Wiki where
  toJSON = wrapFetcher "wiki" . genericToJSON defaultOptions

instance FromJSON Wiki where
  parseJSON = genericParseJSON defaultOptions

fetchWiki :: Fetcher Wiki
fetchWiki = Fetcher {..}
  where
    getRev _ _ _ = return ""
    prefetch name Wiki {..} _ =
      handleAll $ prefetchWith name "nix-prefetch-url" [ T.unpack fullurl ]
      where
        fullurl = fromMaybe defaultUrl url
        defaultUrl = "http://www.emacswiki.org/emacs/download/" <> name <> ".el"
