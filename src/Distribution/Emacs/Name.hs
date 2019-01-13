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


module Distribution.Emacs.Name ( Name (..) ) where

import Data.Data (Data)
import Data.Hashable ( Hashable )
import Data.Text ( Text )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc ( Pretty (..) )
import qualified Data.Text.Prettyprint.Doc as Pretty


newtype Name = Name { fromName :: Text }
  deriving (Data, Eq, Generic, Hashable, Ord, Show, Typeable)


instance Pretty Name where
  pretty = Pretty.pretty . fromName
