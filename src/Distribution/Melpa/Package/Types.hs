{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package.Types where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Distribution.Melpa.Recipe
import Distribution.Melpa.Version
