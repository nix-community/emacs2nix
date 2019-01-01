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
import Nix.Expr
import qualified Nix.Pretty
import Prelude hiding ( (<$>) )
import qualified System.IO.Streams as S
import Text.PrettyPrint.ANSI.Leijen hiding ( sep )

import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Nix.Name as Nix
import System.IO.Streams.Pretty as Pretty

writeIndex :: FilePath  -- ^ output file
           -> Map Nix.Name NExpr
           -> IO ()
writeIndex output packages = do
  S.withFileAsOutput output (write (packageIndex packages))
  where
    write index out = do
      let rendered = renderSmart 1.0 80 (Nix.Pretty.prettyNix index)
      displayStream rendered =<< S.encodeUtf8 out

packageIndex :: Map Nix.Name NExpr -> NExpr
packageIndex (Map.toList -> packages) =
    mkFunction args body
  where
    args = mkParamset [("callPackage", Nothing)] False
    body = (mkNonRecSet . map bindPackage) packages
    bindPackage (Nix.Name { ename }, drv) =
      ("\"" <> Emacs.fromName ename <> "\"") $= drv
