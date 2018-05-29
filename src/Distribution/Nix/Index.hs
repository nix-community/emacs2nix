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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Nix.Index ( readIndex, writeIndex ) where

import Control.Exception
import Data.Fix
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Text ( Text )
import qualified Data.Text as T
import Nix.Parser ( Result(..), parseNixFile )
import Nix.Pretty ( prettyNix )
import Nix.Expr
import System.Directory ( doesFileExist )
import System.IO.Streams ( OutputStream )
import qualified System.IO.Streams as S
import Text.PrettyPrint.ANSI.Leijen hiding ( (<$>) )

import Distribution.Nix.Exception
import Distribution.Nix.Name ( Name, fromName, fromText )

readIndex :: FilePath  -- ^ output file
          -> IO (Map Name NExpr)
readIndex output = parseOutputOrDefault =<< doesFileExist output
  where
    die = throwIO (InvalidIndex output)

    parseOutputOrDefault exists
      | exists = do
          result <- parseNixFile output
          case result of
            Failure _ -> die
            Success parsed ->
              maybe die pure (getFunctionBody parsed >>= getPackages)
      | otherwise = pure M.empty

writeIndex :: FilePath  -- ^ output file
           -> Map Name NExpr
           -> IO ()
writeIndex output packages = do
  S.withFileAsOutput output (write (packageIndex packages))
  where
    write index out = do
      let rendered = renderPretty 1 80 (prettyNix index)
      displayStream rendered =<< S.encodeUtf8 out

getFunctionBody :: NExpr -> Maybe NExpr
getFunctionBody (Fix (NAbs _ body)) = Just body
getFunctionBody _ = Nothing

getPackages :: NExpr -> Maybe (Map Name NExpr)
getPackages (Fix (NSet bindings)) =
  let
    getPackage (NamedVar (StaticKey name :| []) expr _) = Just (fromText name, expr)
    getPackage _ = Nothing
  in
    M.fromList <$> traverse getPackage bindings
getPackages _ = Nothing

packageIndex :: Map Name NExpr -> NExpr
packageIndex (M.toList -> packages) = mkFunction args body where
  args = mkParamset [("callPackage", Nothing)] False
  body = (mkNonRecSet . map bindPackage) packages
  bindPackage (name, expr) = bindTo (fromName name) expr

displayStream :: SimpleDoc -> OutputStream Text -> IO ()
displayStream sdoc out = display sdoc where
  display SFail = throwIO PrettyFailed
  display SEmpty = return ()
  display (SChar c sdoc') = S.write (Just (T.singleton c)) out >> display sdoc'
  display (SText _ t sdoc') = S.write (Just (T.pack t)) out >> display sdoc'
  display (SLine i sdoc') = S.write (Just (T.cons '\n' (indentation i))) out >> display sdoc'
  display (SSGR _ sdoc') = display sdoc'
  indentation i = T.replicate i (T.singleton ' ')
