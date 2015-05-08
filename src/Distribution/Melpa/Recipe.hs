{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import Turtle

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Files (Files)
import qualified Distribution.Melpa.Files as Files

data Recipe =
  Recipe
  { fetcher :: Fetcher
  , files :: Maybe Files
  }
  deriving (Eq, Read, Show)

instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \obj -> do
    files <- traverse Files.fromMelpa (HM.lookup "files" obj)
    fetcher <- parseJSON (Object obj)
    return Recipe {..}

instance ToJSON Recipe where
  toJSON Recipe {..} =
    addFiles (toJSON fetcher)
    where
      addFiles (Object obj) = Object (HM.insert "files" (toJSON files) obj)
      addFiles _ = error "addFiles: the impossible happened!"

readRecipes :: FilePath -> IO (HashMap Text Recipe)
readRecipes path =
  S.withFileAsInput (encodeString path) $ \inp -> do
    result <- parseEither parseJSON <$> S.parseFromStream json' inp
    either error return result
