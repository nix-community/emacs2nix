module Distribution.Nix.Hash
       ( HashError(..)
       , hash
       ) where

import Control.Error
import Control.Exception (SomeException)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified System.IO.Streams as S

import Util

data HashError = NoHashError
               | NixHashError Int Text
               | OtherHashError SomeException
  deriving (Show)

hash :: FilePath -> ExceptT HashError IO Text
hash filename
  = runInteractiveProcess "nix-hash" args Nothing Nothing
    OtherHashError NixHashError
    (\out -> do
      hashes <- liftIO (S.lines out >>= S.decodeUtf8 >>= S.toList)
      case hashes of
        (_hash:_) -> pure _hash
        _ -> throwE NoHashError)
  where
    args = [ "--type", "sha256", "--base32", "--flat", filename ]
