{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error
import Control.Exception (bracket)
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
prefetchWith name prefetcher args =
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
             hash <- hoistEither $ headErr (name <> ": could not find hash") hashes
             path <- hoistEither $ headErr (name <> ": could not find path") paths
             return (T.unpack path, hash))
