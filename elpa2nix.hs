{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent (setNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Error
import Control.Exception
import Control.Monad (join, when)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Options.Applicative
import System.FilePath ((</>), (<.>))
import System.IO (hClose)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempFile)

import Paths_emacs2nix

import qualified Distribution.Elpa.Package as Elpa
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Name as Nix
import Distribution.Nix.Package.Elpa (Package)
import qualified Distribution.Nix.Package.Elpa as Nix
import Util

main :: IO ()
main = join (execParser (info (helper <*> parser) desc))
  where
    desc = fullDesc <> progDesc "Generate Nix expressions from ELPA"

parser :: Parser (IO ())
parser =
  elpa2nix
  <$> (threads <|> pure 0)
  <*> output
  <*> server
  where
    threads = option auto (long "threads" <> short 't' <> metavar "N"
                            <> help "use N threads; default is number of CPUs")
    output = strOption (long "output" <> short 'o' <> metavar "FILE"
                        <> help "write output to FILE")
    server = strArgument (metavar "URL"
                          <> help "get packages from server at URL")

elpa2nix :: Int -> FilePath -> String -> IO ()
elpa2nix threads output server = showExceptions_ $ do
  when (threads > 0) (setNumCapabilities threads)

  archives <- getPackages server
  hashedPackages <- runConcurrently
                    (M.traverseWithKey (hashPackage server) archives)
  let
    liftMaybe (x, y) = (,) x <$> y
    (_, packages) = (unzip . mapMaybe liftMaybe . M.toList) hashedPackages

  writePackages output packages

-- * Error types

newtype ArchiveError = ArchiveError SomeException
  deriving (Show, Typeable)

instance Exception ArchiveError

-- * getPackages

getPackages :: String -> IO (Map Text Elpa.Package)
getPackages uri = mapException ArchiveError $ do
  let args = [uri </> "archive-contents"]
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    runInteractiveProcess "curl" args Nothing Nothing $ \out -> do
      tmp <- S.handleToOutputStream h >>= S.atEndOfOutput (hClose h)
      S.connect out tmp
    readArchive path

-- * readArchive

type InputByteStream = S.InputStream ByteString

data ParseArchiveError = ParseArchiveError String
  deriving (Show, Typeable)

instance Exception ParseArchiveError

readArchive :: FilePath -> IO (Map Text Elpa.Package)
readArchive path = mapException ArchiveError $ do
  let
    args = ["--eval", eval]
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"
  emacs args $ \out -> do
    result <- parseJsonFromStream out
    case result of
      Left parseError -> throwIO (ParseArchiveError parseError)
      Right pkgs -> pure pkgs

emacs :: [String] -> (InputByteStream -> IO a) -> IO a
emacs args go = do
  load <- getDataFileName "elpa2json.el"
  let
    args' = [ "--batch", "--load", load ] ++ args
  runInteractiveProcess "emacs" args' Nothing Nothing go

parseJsonFromStream :: FromJSON a => InputByteStream -> IO (Either String a)
parseJsonFromStream stream = parseEither parseJSON <$> S.parseFromStream json' stream

-- * hashPackage

data DistNotImplemented = DistNotImplemented Text
  deriving (Show, Typeable)

instance Exception DistNotImplemented

hashPackage :: String -> Text -> Elpa.Package -> Concurrently (Maybe Package)
hashPackage server name pkg = Concurrently $ showExceptions $ do
  let
    ver = T.intercalate "." (map (T.pack . show) (Elpa.ver pkg))
    basename
      | null (Elpa.ver pkg) = T.unpack name
      | otherwise = T.unpack (name <> "-" <> ver)

  ext <- case Elpa.dist pkg of
           "single" -> pure "el"
           "tar" -> pure "tar"
           other -> throwIO (DistNotImplemented other)
  let
    url = server </> basename <.> ext
    fetch = Nix.URL { Nix.url = T.pack url
                    , Nix.sha256 = Nothing
                    }

  (_, fetcher) <- Nix.prefetch name fetch
  pure Nix.Package { Nix.pname = Nix.fromText name
                   , Nix.ename = name
                   , Nix.version = ver
                   , Nix.fetch = fetcher
                   , Nix.deps = map Nix.fromText (maybe [] M.keys (Elpa.deps pkg))
                   }

-- * writePackages

writePackages :: FilePath -> [Package] -> IO ()
writePackages path pkgs
  = S.withFileAsOutput path $ \out -> do
    enc <- S.fromLazyByteString (encodePretty pkgs)
    S.connect enc out
