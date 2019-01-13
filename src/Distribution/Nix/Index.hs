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

module Distribution.Nix.Index ( writeIndex ) where

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Nix.Expr
import qualified Nix.Pretty
import Prelude hiding ( (<$>) )
import System.IO.Streams (OutputStream)
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Distribution.Emacs.Name as Emacs
import System.IO.Streams.Pretty as Pretty

writeIndex
    :: OutputStream Text
    -> Map Emacs.Name NExpr
    -> IO ()
writeIndex output packages =
  do
    let
      index = packageIndex packages
      rendered =
          Pretty.layoutPretty
              Pretty.defaultLayoutOptions
              (Nix.Pretty.prettyNix index)
    displayStream rendered output

packageIndex :: Map Emacs.Name NExpr -> NExpr
packageIndex (Map.toList -> packages) =
    (mkNonRecSet . map bindPackage) packages
  where
    bindPackage (ename, drv) =
      ("\"" <> Emacs.fromName ename <> "\"") $= drv
