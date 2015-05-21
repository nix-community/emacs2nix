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
import System.Exit (ExitCode(..))
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
prefetchWith _ prefetcher args = handleAll $ EitherT $ do
  bracket
    (S.runInteractiveProcess prefetcher args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, _, out, pid) -> do
           S.write Nothing inp
           lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
           let hashes = mapMaybe (T.stripPrefix "hash is ") lines_
               paths = mapMaybe (T.stripPrefix "path is ") lines_
               anyerr = "; output was:\n" <> T.unlines lines_
           exitCode <- S.waitForProcess pid
           case exitCode of
             ExitFailure errno ->
               return
               $ Left ("prefetcher failed with exit code " <> T.pack (show errno))
             ExitSuccess -> return $ do
               hash_ <- headErr ("could not fild hash" <> anyerr) hashes
               path_ <- headErr ("could not fild path" <> anyerr) paths
               return (T.unpack path_, hash_))

handleAll :: EitherT Text IO a -> EitherT Text IO a
handleAll act =
  EitherT $ handle
    (\(SomeException e) -> return $ Left (T.pack $ show e))
    (runEitherT act)
