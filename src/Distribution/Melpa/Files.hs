{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Files where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
#endif

import Data.Aeson
import Data.Aeson.Types (Parser, defaultOptions, typeMismatch)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import GHC.Generics

data Files =
  Files
  { files :: [FilePath]
  , defaults :: [FilePath]
  , exclude :: [FilePath]
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Files where
  toJSON = genericToJSON defaultOptions

instance FromJSON Files where
  parseJSON = genericParseJSON defaultOptions

instance Monoid Files where
  mempty = Files { files = [], defaults = [], exclude = [] }
  mappend a b =
    Files
    { files = on mappend files a b
    , defaults = on mappend defaults a b
    , exclude = on mappend exclude a b
    }

fromMelpa :: Value -> Parser Files
fromMelpa path@(String _) = (\f -> mempty { files = [f] }) <$> parseJSON path
fromMelpa (Array fs) = foldr mappend mempty <$> traverse fromMelpa fs
fromMelpa (Object obj) = do
  dflts <- fromMaybe mempty <$> traverse fromMelpa (HM.lookup "defaults" obj)
  excl <- fromMaybe mempty <$> traverse fromMelpa (HM.lookup "exclude" obj)
  return mempty
    { defaults = files dflts
    , exclude = files excl
    }
fromMelpa other = typeMismatch "file or files" other
