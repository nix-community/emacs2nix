{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Nix.Hash where

import Control.Exception
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified System.IO.Streams as S

import Util

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
