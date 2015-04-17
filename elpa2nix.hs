{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception (SomeException(..), bracket_, handle)
import Crypto.Hash
  (Digest, HashAlgorithm(..), SHA256, digestToHexByteString, hashUpdate)
import Data.Aeson
import Data.Aeson.Encode (encodeToByteStringBuilder)
import Data.Aeson.Types hiding (Options)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import GHC.Generics
import qualified Network.Http.Client as Http
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO (hClose, hPutStrLn, stderr)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp (withSystemTempFile)

import Paths_elpa2nix

main :: IO ()
main = do
  opts@(Options {..}) <- getOptions
  getArchives opts >>= writePackages output

data Package =
  Package
  { ver :: [Integer]
  , deps :: Maybe (Map Text [Integer])
  , desc :: Text
  , dist :: Text -- TODO: replace with an enumeration
  , hash :: Maybe Text
  , archive :: Maybe String
  , broken :: Maybe Bool
  }
  deriving (Eq, Generic, Read, Show)

instance FromJSON Package where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Package where
  toJSON = genericToJSON defaultOptions

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

die :: String -> IO ()
die str = hPutStrLn stderr str >> exitFailure

getArchives :: Options -> IO (Map Text Package)
getArchives Options {..} = do
  archives <- runConcurrently $ for uris $ \uri -> Concurrently (getPackages uri)
  oldPkgs <- readPackages input
  let pkgs = foldr (M.unionWith keepLatestVersion) oldPkgs archives
  sem <- newQSem threads
  runConcurrently $ M.traverseWithKey (hashPackage oldPkgs sem) pkgs
  where
    keepLatestVersion a b =
      case comparing ver a b of
        LT -> b
        GT -> a
        EQ -> b

getPackages :: String -> IO (Map Text Package)
getPackages uri = do
  Http.get (B8.pack $ uri </> "archive-contents") $ \_ str -> do
    withSystemTempFile "elpa2nix-archive-contents-" $ \path _h -> do
      _h <- S.handleToOutputStream _h >>= S.atEndOfOutput (hClose _h)
      S.supply str _h
      S.write Nothing _h
      M.map setArchive <$> readArchive path
  where
    setArchive pkg = pkg { archive = Just uri }

readArchive :: FilePath -> IO (Map Text Package)
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

readPackages :: FilePath -> IO (Map Text Package)
readPackages path =
  handle (\(SomeException _) -> return M.empty)
    $ fromMaybe M.empty <$> S.withFileAsInput path parseJsonFromStream

parseJsonFromStream :: FromJSON a => S.InputStream ByteString -> IO (Maybe a)
parseJsonFromStream stream = parseMaybe parseJSON <$> S.parseFromStream json' stream

hashPackage :: Map Text Package -> QSem -> Text -> Package -> Concurrently Package
hashPackage pkgs sem name pkg =
  Concurrently $ handle brokenPkg $
  case M.lookup name pkgs of
    Just pkg' | isJust (hash pkg') -> return pkg' { desc = "" }
    _ -> do
      let uri = fromMaybe (error "missing archive URI") (archive pkg)
          filename = T.unpack name ++ version_
          version_ =
            case ver pkg of
              [] -> ""
              vers -> "-" ++ intercalate "." (map show vers)
          ext = case dist pkg of
                  "single" -> "el"
                  "tar" -> "tar"
                  other -> error $ "unrecognized distribution type " ++ T.unpack other
      hash_ <- withQSem sem $ Http.get (B8.pack $ uri </> filename <.> ext) $ \_ -> sha256
      return pkg { hash = Just hash_, desc = "" }
  where
    nameS = T.unpack name
    brokenPkg (SomeException e) = do
      putStrLn $ "marking " ++ nameS ++ " broken due to exception:\n" ++ show e
      return pkg { broken = Just True }

writePackages :: FilePath -> Map Text Package -> IO ()
writePackages path pkgs =
  S.withFileAsOutput path $ \_out -> do
    _out <- S.builderStream _out
    S.write (Just builder) _out
    S.write Nothing _out
  where builder = encodeToByteStringBuilder (toJSON pkgs)

withQSem :: QSem -> IO a -> IO a
withQSem qsem go = bracket_ (waitQSem qsem) (signalQSem qsem) go

sha256 :: InputStream ByteString -> IO Text
sha256 stream = do
  ctx <- S.fold hashUpdate hashInit stream
  let dig :: Digest SHA256
      dig = hashFinalize ctx
  return $ T.decodeUtf8 (digestToHexByteString dig)
