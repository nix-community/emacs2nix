{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Nix.Exception where

import Control.Exception
import Data.Typeable

data NixError = InvalidIndex FilePath
              | PrettyFailed
  deriving (Show, Typeable)

instance Exception NixError
