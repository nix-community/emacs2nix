{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Control.Error
import Control.Exception (SomeException(..), bracket, handle)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Char (isHexDigit)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Environment (getEnvironment)
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
prefetchWith _ prefetcher args = handleAll $ EitherT $ do
  oldEnv <- HM.fromList <$> getEnvironment
  let env = HM.toList (HM.insert "QUIET" "1" oldEnv)
  bracket
    (S.runInteractiveProcess prefetcher args Nothing (Just env))
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
           S.write Nothing inp
           lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
           let hashes = catMaybes (map getHash lines_)
           return $ headErr ("could not find hash in:\n" <> T.unlines lines_) hashes)
  where
    getHash :: Text -> Maybe Text
    getHash txt = do
        let (hash_, rest_) = T.break (== ' ') txt
        if T.null rest_ && not (T.null hash_)
          then Just hash_
          else Nothing

handleAll :: EitherT Text IO a -> EitherT Text IO a
handleAll act =
  EitherT $ handle
    (\(SomeException e) -> return $ Left (T.pack $ show e))
    (runEitherT act)
