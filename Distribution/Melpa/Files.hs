{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Files where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import Data.Monoid

data File = File FilePath | Dir Files | Defaults Files | Exclude Files
  deriving (Eq, Read, Show)

instance ToJSON File where
  toJSON (File path) = toJSON path
  toJSON (Dir files) = toJSON files
  toJSON (Defaults path) = Object (HM.fromList [("defaults", toJSON path)])
  toJSON (Exclude path) = Object (HM.fromList [("exclude", toJSON path)])

instance FromJSON File where
  parseJSON files@(Array _) = Dir <$> parseJSON files
  parseJSON file@(String _) = File <$> parseJSON file
  parseJSON (Object obj) = do
    dflt <- obj .:? "defaults"
    excl <- obj .:? "exclude"
    case dflt of
      Nothing -> case excl of
        Nothing -> fail "expected defaults or exclude"
        Just path -> return (Exclude path)
      Just path -> return (Defaults path)
  parseJSON other = typeMismatch "file" other

newtype Files = Files [File]
  deriving (Eq, Monoid, Read, Show)

instance ToJSON Files where
  toJSON (Files files) = toJSON files

instance FromJSON Files where
  parseJSON files@(Array _) = Files <$> parseJSON files
  parseJSON file@(Object _) = Files . (: []) <$> parseJSON file
  parseJSON file@(String _) = Files . (: []) <$> parseJSON file
  parseJSON other = typeMismatch "files" other
