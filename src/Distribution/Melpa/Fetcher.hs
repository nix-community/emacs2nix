{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error
import Control.Exception (SomeException(..), bracket, handle)
import Control.Monad (liftM)
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
  , prefetch :: Text -> f -> Text -> EitherT Text IO (FilePath, Text)
  }
  deriving Generic

wrapFetcher :: Text -> Value -> Value
wrapFetcher fetch val =
  case val of
    (Object obj) -> Object (HM.insert "fetcher" (toJSON fetch) obj)
    _ -> error "wrapFetcher: not a fetcher object!"

prefetchWith :: Text -> FilePath -> [String] -> EitherT Text IO (FilePath, Text)
prefetchWith _ prefetcher args =
  EitherT $ bracket
    (S.runInteractiveProcess prefetcher args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           let getHash = T.stripPrefix "hash is "
               getPath = T.stripPrefix "path is "
           hashes <- liftM (mapMaybe getHash) $ S.lines out >>= S.decodeUtf8 >>= S.toList
           paths <- liftM (mapMaybe getPath) $ S.lines out >>= S.decodeUtf8 >>= S.toList
           runEitherT $ do
             hash <- hoistEither $ headErr ("could not find hash with " <> cmd) hashes
             path <- hoistEither $ headErr ("could not find path with " <> cmd) paths
             return (T.unpack path, hash))
  where
    cmd = T.pack $ S.showCommandForUser prefetcher args

handleAll :: EitherT Text IO a -> EitherT Text IO a
handleAll act =
  EitherT $ handle
    (\(SomeException e) -> return $ Left (T.pack $ show e))
    (runEitherT act)
