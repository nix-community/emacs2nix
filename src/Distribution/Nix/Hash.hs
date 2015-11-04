module Distribution.Nix.Hash (hash) where

import Control.Error
import Data.Text (Text)
import qualified System.IO.Streams as S

import Util

hash :: FilePath -> ExceptT String IO Text
hash filename
  = runInteractiveProcess "nix-hash" args Nothing Nothing
    (\out -> ExceptT $ do
         hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
         case hashes of
           (_hash:_) -> return (Right _hash)
           _ -> return (Left "unable to hash"))
  where
    args = [ "--type", "sha256", "--base32", "--flat", filename ]
