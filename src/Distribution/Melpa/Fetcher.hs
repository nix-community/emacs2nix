{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error
import Control.Exception (SomeException(..), bracket, handle)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified System.IO.Streams as S

data Fetcher f =
  Fetcher
  { getRev :: Text -> f -> FilePath -> EitherT Text IO Text
  , prefetch :: Text -> f -> Text -> EitherT Text IO Text
  }
  deriving Generic

wrapFetcher :: Text -> Value -> Value
wrapFetcher fetch val =
  case val of
    (Object obj) -> Object (HM.insert "fetcher" (toJSON fetch) obj)
    _ -> error "wrapFetcher: not a fetcher object!"

prefetchWith :: Text -> FilePath -> [String] -> EitherT Text IO Text
prefetchWith _ prefetcher args =
  handleAll $ EitherT $ bracket
    (S.runInteractiveProcess prefetcher args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           hash <- S.fold (<>) T.empty =<< S.decodeUtf8 out
           return $ if T.length hash /= 52
             then Left ("not a base32-encoded sha256: " <> hash <> "\n" <> anyerr)
             else Right hash)
  where
    cmd = T.pack $ S.showCommandForUser prefetcher args
    anyerr = "tried prefetch command: " <> cmd

handleAll :: EitherT Text IO a -> EitherT Text IO a
handleAll act =
  EitherT $ handle
    (\(SomeException e) -> return $ Left (T.pack $ show e))
    (runEitherT act)
