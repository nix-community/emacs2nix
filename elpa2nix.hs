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
import Control.Exception (SomeException(..), handle)
import Control.Monad (join, when)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
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

elpa2nix :: Int -> FilePath -> String -> IO ()
elpa2nix threads output server = do
  when (threads > 0) (setNumCapabilities threads)

  archives <- runConcurrently $ Concurrently (getPackages server)
  packages <- M.fromList . mapMaybe liftMaybe . M.toList
              <$> runConcurrently (M.traverseWithKey (hashPackage server) archives)

  writePackages output (Nix.cleanNames packages)
  where
    liftMaybe (x, y) = (,) x <$> y

getPackages :: String -> IO (Map Text Elpa.Package)
getPackages uri = do
  let args = [uri </> "archive-contents"]
  (_, contents, _, pid) <- S.runInteractiveProcess "curl" args Nothing Nothing
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    tmp <- S.handleToOutputStream h >>= S.atEndOfOutput (hClose h)
    S.connect contents tmp
    _ <- S.waitForProcess pid
    readArchive path

readArchive :: FilePath -> IO (Map Text Elpa.Package)
readArchive path = do
  load <- getDataFileName "elpa2json.el"
  (_, out, _, pid) <- S.runInteractiveProcess "emacs"
                      ["--batch", "--load", load, "--eval", eval]
                      Nothing Nothing
  Just pkgs <- parseJsonFromStream out
  S.waitForProcess pid >> return pkgs
  where
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"

parseJsonFromStream :: FromJSON a => S.InputStream ByteString -> IO (Maybe a)
parseJsonFromStream stream = parseMaybe parseJSON <$> S.parseFromStream json' stream

hashPackage :: String -> Text -> Elpa.Package -> Concurrently (Maybe Package)
hashPackage server name pkg = Concurrently $ handle brokenPkg $ do
  let ver = T.intercalate "." (map (T.pack . show) (Elpa.ver pkg))
      basename
        | null (Elpa.ver pkg) = T.unpack name
        | otherwise = T.unpack (name <> "-" <> ver)
      ext = case Elpa.dist pkg of
              "single" -> "el"
              "tar" -> "tar"
              other -> error (nameS ++ ": unrecognized distribution type " ++ T.unpack other)
      url = server </> basename <.> ext
  let fetch = Nix.URL { Nix.url = T.pack url
                      , Nix.sha256 = Nothing
                      }
  Right (_, fetcher) <- runExceptT (Nix.prefetch name fetch)
  return $ Just Nix.Package
    { Nix.version = ver
    , Nix.fetch = fetcher
    , Nix.deps = maybe [] M.keys (Elpa.deps pkg)
    }
  where
    nameS = T.unpack name
    brokenPkg (SomeException e) = do
      putStrLn $ nameS ++ ": encountered exception\n" ++ show e
      return Nothing

writePackages :: FilePath -> Map Text Package -> IO ()
writePackages path pkgs =
  S.withFileAsOutput path $ \out -> do
    enc <- S.fromLazyByteString (encodePretty pkgs)
    S.connect enc out
