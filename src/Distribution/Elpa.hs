{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2016 Thomas Tuegel

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

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
