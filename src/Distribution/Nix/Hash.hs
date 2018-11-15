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

module Distribution.Nix.Hash where

import Control.Exception
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified System.IO.Streams as S

import Process

data NoHashOutput = NoHashOutput
  deriving (Show, Typeable)

instance Exception NoHashOutput

newtype NixHashError = NixHashError SomeException
  deriving (Show, Typeable)

instance Exception NixHashError

hash :: FilePath -> IO Text
hash filename
  = mapException NixHashError
    (runInteractiveProcess "nix-hash" args Nothing Nothing getHash)
  where
    args = [ "--type", "sha256", "--base32", "--flat", filename ]
    getHash out = do
      hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
      case hashes of
        (theHash : _) -> pure theHash
        _ -> throwIO NoHashOutput
