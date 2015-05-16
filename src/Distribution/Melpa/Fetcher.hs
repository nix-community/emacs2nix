{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

type family Rev f

data Fetcher f =
  Fetcher
  { getRev :: f -> EitherT Text IO (Rev f)
  , prefetch :: Text -> f -> Rev f -> EitherT Text IO (FilePath, Text)
  }
  deriving Generic

wrapFetcher :: Text -> Value -> Value
wrapFetcher fetch val =
  case val of
    (Object obj) -> Object (HM.insert "fetcher" (toJSON fetch) obj)
    _ -> error "wrapFetcher: not a fetcher object!"
