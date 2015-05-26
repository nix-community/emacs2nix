{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Async (Concurrently(..))
import Control.Exception (SomeException(..), handle)
import Data.Aeson (FromJSON(..), json')
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Network.Http.Client as Http
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>))
import System.IO (hClose)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempFile)

import Paths_elpa2nix

import qualified Distribution.Elpa.Package as Elpa
import qualified Distribution.Nix.Package as Nix

main :: IO ()
main = do
  opts@(Options {..}) <- getOptions
  getArchives opts >>= writePackages output

data Options =
  Options
  { output :: FilePath
  , input :: FilePath
  , uris :: [String]
  , threads :: Int
  }

getOptions :: IO Options
getOptions = do
  args <- getArgs
  (opts, uris_) <-
       case getOpt Permute optdescr args of
         (opts, uris_, []) -> return (foldl (flip id) dftl opts, uris_)
         (_, _, errs) -> error (concat errs ++ usageInfo header optdescr)
  ncap <- getNumCapabilities
  return opts
    { uris = uris_
    , threads = if threads opts == 0 then ncap else threads opts
    , input = if input opts == "" then output opts else input opts
    }
  where
    header = "Usage: elpa2nix [OPTION...] URIs..."
    dftl = Options { output = "", uris = [], threads = 0, input = "" }
    optdescr =
      [ Option ['o'] [] (ReqArg setOutput "FILE") "output FILE"
      , Option ['i'] [] (ReqArg setInput "FILE") "input FILE (defaults to output)"
      , Option ['t'] [] (ReqArg setThreads "N") "use N threads"
      ]
    setOutput out opts = opts { output = out }
    setInput inp opts = opts { input = inp }
    setThreads n opts = opts { threads = read n }

getArchives :: Options -> IO (Map Text Nix.Package)
getArchives Options {..} = do
  archives <- runConcurrently $ for uris $ \uri -> Concurrently (getPackages uri)
  oldPkgs <- readPackages input
  let pkgs = foldr (M.unionWith keepLatestVersion) oldPkgs archives
  M.fromList . mapMaybe liftMaybe . M.toList
    <$> runConcurrently (M.traverseWithKey hashPackage pkgs)
  where
    liftMaybe (x, y) = (,) x <$> y
    keepLatestVersion a b =
      case comparing Elpa.ver a b of
        LT -> b
        GT -> a
        EQ -> b

getPackages :: String -> IO (Map Text Elpa.Package)
getPackages uri = do
  Http.get (B8.pack $ uri </> "archive-contents") $ \_ str -> do
    withSystemTempFile "elpa2nix-archive-contents-" $ \path _h -> do
      _h <- S.handleToOutputStream _h >>= S.atEndOfOutput (hClose _h)
      S.supply str _h
      S.write Nothing _h
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

readPackages :: FilePath -> IO (Map Text Elpa.Package)
readPackages path =
  handle (\(SomeException _) -> return M.empty)
    $ fromMaybe M.empty <$> S.withFileAsInput path parseJsonFromStream

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
