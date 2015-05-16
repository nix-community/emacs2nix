{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.Fossil ( Fossil, fetchFossil ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Fossil =
  Fossil
  { url :: Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev Fossil = ()

instance ToJSON Fossil where
  toJSON = wrapFetcher "fossil" . genericToJSON defaultOptions

instance FromJSON Fossil where
  parseJSON = genericParseJSON defaultOptions

fetchFossil :: Fetcher Fossil
fetchFossil = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ _ name _ _ = left (name <> ": fetcher 'fossil' not implemented")
-}
