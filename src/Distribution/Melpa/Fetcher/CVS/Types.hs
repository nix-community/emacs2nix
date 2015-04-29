{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher.CVS.Types where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

data CVS =
  Fetcher
  { url :: Text
  , branch :: Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON CVS where
  toJSON = renameFields . genericToJSON defaultOptions
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
