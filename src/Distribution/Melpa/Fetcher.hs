{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error hiding (err)
import Control.Exception (SomeException(..), bracket, handle)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((<.>))
import qualified System.IO.Streams as S

import Paths_melpa2nix (getDataFileName)

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
  realPrefetcher <- getDataFileName (prefetcher <.> "sh")
  oldEnv <- HM.fromList <$> getEnvironment
  let env = HM.toList (HM.insert "PRINT_PATH" "1" oldEnv)
  bracket
    (S.runInteractiveProcess realPrefetcher args Nothing (Just env))
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, err, pid) -> do
           S.write Nothing inp
           _lines <- S.lines out >>= S.decodeUtf8 >>= S.toList
           errors <- T.unlines <$> (S.lines err >>= S.decodeUtf8 >>= S.toList)
           let anyerr = "; output was:\n" <> T.unlines _lines
           exitCode <- S.waitForProcess pid
           case exitCode of
             ExitFailure errno ->
               return
               $ Left ("prefetcher failed with exit code "
                       <> T.pack (show errno)
                       <> ":\n" <> errors <> "\n")
             ExitSuccess -> return $ do
               hash_ <- headErr ("could not find hash" <> anyerr) _lines
               _lines <- return (tail _lines)
               path_ <- headErr ("could not find path" <> anyerr) _lines
               return (T.unpack path_, hash_))

handleAll :: EitherT Text IO a -> EitherT Text IO a
handleAll act =
  EitherT $ handle
    (\(SomeException e) -> return $ Left (T.pack $ show e))
    (runEitherT act)
