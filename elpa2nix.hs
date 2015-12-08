{-# LANGUAGE CPP #-}
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
import Control.Exception (SomeException(..))
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO (hClose)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempFile)

import Paths_emacs2nix

import qualified Distribution.Elpa.Package as Elpa
import qualified Distribution.Nix.Fetch as Nix
import Distribution.Nix.Package.Elpa (Package)
import qualified Distribution.Nix.Package.Elpa as Nix

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

showExceptions :: Show e => ExceptT e IO b -> IO (Maybe b)
showExceptions go = do
  result <- runExceptT go
  case result of
    Right a -> pure (Just a)
    Left e -> do
      S.write (Just (T.pack (show e))) =<< S.encodeUtf8 S.stdout
      pure Nothing

showExceptions_ :: Show e => ExceptT e IO b -> IO ()
showExceptions_ go = showExceptions go >> pure ()

elpa2nix :: Int -> FilePath -> String -> IO ()
elpa2nix threads output server = showExceptions_ $ do
  when (threads > 0) (lift (setNumCapabilities threads))

  archives <- getPackages server
  hashedPackages <- (lift . runConcurrently) (M.traverseWithKey (hashPackage server) archives)
  let
    liftMaybe (x, y) = (,) x <$> y
    packages = (M.fromList . mapMaybe liftMaybe . M.toList) hashedPackages

  writePackages output (Nix.cleanNames packages)

-- * Error types

data ArchiveError = DownloadArchiveError Int Text
                  | ContentsArchiveError Int Text
                  | ParseArchiveError Text
                  | OtherArchiveError SomeException
  deriving (Show)

-- * getPackages

getPackages :: String -> ExceptT ArchiveError IO (Map Text Elpa.Package)
getPackages uri = ExceptT $ do
  let args = [uri </> "archive-contents"]
  (_, contents, errors, pid) <- S.runInteractiveProcess "curl" args Nothing Nothing
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    tmp <- S.handleToOutputStream h >>= S.atEndOfOutput (hClose h)
    S.connect contents tmp
    let
      getErrors = Concurrently (S.decodeUtf8 errors >>= S.fold (<>) T.empty)
      wait = Concurrently (S.waitForProcess pid)
    (message, exit) <- runConcurrently ((,) <$> getErrors <*> wait)
    case exit of
      ExitSuccess -> runExceptT (readArchive path)
      ExitFailure code -> pure (Left (DownloadArchiveError code message))

-- * readArchive

type InputByteStream = S.InputStream ByteString
type OutputByteStream = S.OutputStream ByteString

readArchive :: FilePath -> ExceptT ArchiveError IO (Map Text Elpa.Package)
readArchive path = do
  let
    args = ["--eval", eval]
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"
  (_, out, errors, pid) <- lift (emacs args)
  let
    getOutput = Concurrently (parseJsonFromStream out)
    getErrors = Concurrently (S.decodeUtf8 errors >>= S.fold (<>) T.empty)
    wait = Concurrently (S.waitForProcess pid)
  (json, message, exit) <- (lift . runConcurrently)
                           ((,,) <$> getOutput <*> getErrors <*> wait)
  case exit of
    ExitSuccess ->
      case json of
        Left parseError -> throwE (ParseArchiveError (T.pack parseError))
        Right pkgs -> pure pkgs
    ExitFailure code -> throwE (ContentsArchiveError code message)

emacs :: [String]
      -> IO (OutputByteStream, InputByteStream, InputByteStream, S.ProcessHandle)
emacs args = do
  load <- getDataFileName "elpa2json.el"
  let
    args' = [ "--batch", "--load", load ] ++ args
  S.runInteractiveProcess "emacs" args' Nothing Nothing

parseJsonFromStream :: FromJSON a => InputByteStream -> IO (Either String a)
parseJsonFromStream stream = parseEither parseJSON <$> S.parseFromStream json' stream

-- * hashPackage

data PackageError = UnknownDist Text Text
                  | PrefetchError Text String
  deriving (Show)

hashPackage :: String -> Text -> Elpa.Package -> Concurrently (Maybe Package)
hashPackage server name pkg = Concurrently $ showExceptions $ do
  let
    ver = T.intercalate "." (map (T.pack . show) (Elpa.ver pkg))
    basename
      | null (Elpa.ver pkg) = T.unpack name
      | otherwise = T.unpack (name <> "-" <> ver)

  ext <-  case Elpa.dist pkg of
            "single" -> pure "el"
            "tar" -> pure "tar"
            other -> throwE (UnknownDist name other)
  let
    url = server </> basename <.> ext
    fetch = Nix.URL { Nix.url = T.pack url
                    , Nix.sha256 = Nothing
                    }

  prefetch <- (lift . runExceptT) (Nix.prefetch name fetch)
  case prefetch of
    Left fetchError -> throwE (PrefetchError name fetchError)
    Right (_, fetcher) ->
      pure $ Nix.Package { Nix.version = ver
                         , Nix.fetch = fetcher
                         , Nix.deps = maybe [] M.keys (Elpa.deps pkg)
                         }

-- * writePackages

writePackages :: FilePath -> Map Text Package -> ExceptT ArchiveError IO ()
writePackages path pkgs = lift $ do
  S.withFileAsOutput path $ \out -> do
    enc <- S.fromLazyByteString (encodePretty pkgs)
    S.connect enc out
