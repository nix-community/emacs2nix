{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent (forkIO, setNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Exception (SomeException(..), handle)
import Control.Monad (join, when)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
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
import qualified Distribution.Nix.Package as Nix

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
              <$> runConcurrently (M.traverseWithKey hashPackage archives)

  writePackages output packages
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
    M.map setArchive <$> readArchive path
  where
    setArchive pkg = pkg { Elpa.archive = Just uri }

readArchive :: FilePath -> IO (Map Text Elpa.Package)
readArchive path = do
  load <- getDataFileName "elpa2json.el"
  (_, out, err, pid) <- S.runInteractiveProcess "emacs"
                      ["--batch", "--load", load, "--eval", eval]
                      Nothing Nothing
  _ <- forkIO $ S.supply err S.stderr
  Just pkgs <- parseJsonFromStream out
  S.waitForProcess pid >> return pkgs
  where
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"

parseJsonFromStream :: FromJSON a => S.InputStream ByteString -> IO (Maybe a)
parseJsonFromStream stream = parseMaybe parseJSON <$> S.parseFromStream json' stream

hashPackage :: Text -> Elpa.Package -> Concurrently (Maybe Nix.Package)
hashPackage name pkg = Concurrently $ handle brokenPkg $ do
  let ver = T.intercalate "." (map (T.pack . show) (Elpa.ver pkg))
      basename
        | null (Elpa.ver pkg) = T.unpack name
        | otherwise = T.unpack (name <> "-" <> ver)
      ext = case Elpa.dist pkg of
              "single" -> "el"
              "tar" -> "tar"
              other -> error (nameS ++ ": unrecognized distribution type " ++ T.unpack other)
      url = fromJust (Elpa.archive pkg) </> basename <.> ext
  sha256 <- prefetchURL url
  return $ Just Nix.Package
    { Nix.ver = ver
    , Nix.deps = maybe [] M.keys (Elpa.deps pkg)
    , Nix.fetch = Nix.FetchURL
                  { Nix.url = T.pack url
                  , Nix.sha256 = sha256
                  }
    , Nix.build = Nix.ElpaPackage
    }
  where
    nameS = T.unpack name
    brokenPkg (SomeException e) = do
      putStrLn $ nameS ++ ": encountered exception\n" ++ show e
      return Nothing

prefetchURL :: FilePath -> IO Text
prefetchURL url = do
  (inp, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-url" [url] Nothing Nothing
  S.write Nothing inp
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    [] -> S.supply err S.stderr >> error ("unable to prefetch " ++ url)
    (sha256:_) -> return sha256

writePackages :: FilePath -> Map Text Nix.Package -> IO ()
writePackages path pkgs =
  S.withFileAsOutput path $ \out -> do
    enc <- S.fromLazyByteString (encodePretty pkgs)
    S.connect enc out
