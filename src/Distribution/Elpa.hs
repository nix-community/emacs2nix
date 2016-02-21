{-# LANGUAGE DeriveGeneric #-}

module Distribution.Elpa ( Elpa(..) ) where

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import Data.Map.Strict ( Map )
import Data.Text ( Text )
import GHC.Generics

data Elpa =
  Elpa
  { ver :: [Integer]
  , deps :: Maybe (Map Text [Integer])
  , dist :: Text -- TODO: replace with an enumeration
  , broken :: Maybe Bool
  }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Elpa
instance ToJSON Elpa
