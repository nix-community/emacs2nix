{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.CVS ( CVS, fetchCVS ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Fetcher

data CVS =
  CVS
  { url :: Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON CVS where
  toJSON = wrapFetcher "cvs" . renameFields . genericToJSON defaultOptions
    where renameFields (Object obj) =
            let renameFields_go ("branch", b) = ("module", b)
                renameFields_go other = other
            in Object (HM.fromList (map renameFields_go (HM.toList obj)))
          renameFields other = other

instance FromJSON CVS where
  parseJSON = genericParseJSON defaultOptions . renameFields
    where renameFields (Object obj) =
            let renameFields_go ("module", b) = ("branch", b)
                renameFields_go other = other
            in Object (HM.fromList (map renameFields_go (HM.toList obj)))
          renameFields other = other

fetchCVS :: Fetcher CVS
fetchCVS = Fetcher {..}
  where
    getRev _ _ _ = return ""
    prefetch name CVS {..} _ =
      prefetchWith name "nix-prefetch-cvs" [ T.unpack url, T.unpack (fromMaybe name branch) ]
