{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Archive where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Aeson
import Data.Aeson.Types (defaultOptions, parseEither)
import Data.HashMap.Strict (HashMap)
import Filesystem.Path.CurrentOS (encodeString)
import GHC.Generics
import Prelude hiding (FilePath)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import Turtle

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

readArchive :: FilePath -> IO (HashMap Text Archive)
readArchive path =
  S.withFileAsInput (encodeString path) $ \inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result
