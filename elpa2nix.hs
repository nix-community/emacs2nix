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
import Control.Exception
import Control.Monad (filterM, join, unless, when)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Data.Typeable (Typeable)
import Options.Applicative
import System.Directory
       ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents )
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
import Distribution.Nix.Pretty hiding ((</>))
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
  <*> indexOnly
  where
    threads = option auto
              (long "threads" <> short 't'
               <> metavar "N"
               <> help "use N threads; default is number of CPUs")
    output = strOption
             (long "output" <> short 'o'
              <> metavar "FILE"
              <> help "write output to FILE")
    server = strArgument
             (metavar "URL"
              <> help "get packages from server at URL")
    indexOnly = flag False True
                (long "index-only"
                 <> help "don't update packages, only update the index file")

elpa2nix :: Int -> FilePath -> String -> Bool -> IO ()
elpa2nix threads output server indexOnly = showExceptions_ $ do
  when (threads > 0) (setNumCapabilities threads)

  archives <- getPackages server

  (unless indexOnly . runConcurrently)
    (mapM_ (updatePackage server output) (M.toList archives))

  updateIndex output

updateIndex :: FilePath -> IO ()
updateIndex output = do
  createDirectoryIfMissing True output

  let
    packageNamesOnly path
      | "." `isPrefixOf` path = pure False -- skip special files
      | otherwise = doesDirectoryExist (output </> path)
                    -- each packages is a directory
  contents <- getDirectoryContents output >>= filterM packageNamesOnly

  let
    pnames = map T.pack contents
    writeIndex out = do
      let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80)
                (pretty (Nix.packageSet pnames))
      encoded <- S.fromLazyByteString lbs
      S.connect encoded out
    file = output </> "default.nix"
  S.withFileAsOutput file writeIndex

updatePackage :: String -> FilePath -> (Text, Elpa.Package)
              -> Concurrently ()
updatePackage server output elpa = Concurrently $ do
  hashed <- hashPackage server elpa
  case hashed of
    Nothing -> pure ()

    Just pkg -> do
      let dir = output </> (T.unpack . Nix.fromName) (Nix.pname pkg)
      createDirectoryIfMissing True dir
      let
        writePackage out = do
          let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80)
                    (pretty pkg)
          encoded <- S.fromLazyByteString lbs
          S.connect encoded out
        file = dir </> "default.nix"
      S.withFileAsOutput file writePackage

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

hashPackage :: String -> (Text, Elpa.Package) -> IO (Maybe Package)
hashPackage server (name, pkg) = showExceptions $ do
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
  pure Nix.Package
    { Nix.pname = Nix.fromText name
    , Nix.ename = name
    , Nix.version = ver
    , Nix.fetch = fetcher
    , Nix.deps = map Nix.fromText (maybe [] M.keys (Elpa.deps pkg))
    }
