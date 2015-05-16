{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.Darcs ( Darcs, fetchDarcs ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Darcs =
  Darcs
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev Darcs = ()

instance ToJSON Darcs where
  toJSON = wrapFetcher "darcs" . genericToJSON defaultOptions

instance FromJSON Darcs where
  parseJSON = genericParseJSON defaultOptions

fetchDarcs :: Fetcher Darcs
fetchDarcs = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ _ name _ _ = left (name <> ": fetcher 'darcs' not implemented")
-}
