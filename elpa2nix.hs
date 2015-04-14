{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Codec.Compression.GZip as GZ
import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception (SomeException(..), bracket_, handle)
import Control.Lens hiding ((<.>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.Generics
import qualified Network.Wreq.Lens as W
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as W
import OpenSSL.Digest (MessageDigest(SHA256), toHex)
import OpenSSL.Digest.ByteString.Lazy (digest)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO (hClose, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcess)

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
  parseJSON = JSON.genericParseJSON JSON.defaultOptions

instance ToJSON Package where
  toJSON = JSON.genericToJSON JSON.defaultOptions

data Options =
  Options
  { output :: FilePath
  , uris :: [String]
  , threads :: Int
  }

getOptions :: IO Options
getOptions = do
  args <- getArgs
  (opts, uris_) <-
       case getOpt Permute optdescr args of
         (opts, uris_, []) ->
           return (foldl (flip id) defaultOptions opts, uris_)
         (_, _, errs) -> do
           error (concat errs ++ usageInfo header optdescr)
  ncap <- getNumCapabilities
  return opts
    { uris = uris_
    , threads = if threads opts == 0 then ncap else threads opts
    }
  where
    header = "Usage: elpa2nix [OPTION...] URIs..."
    defaultOptions = Options { output = "", uris = [], threads = 0 }
    optdescr =
      [ Option ['o'] [] (ReqArg setOutput "FILE") "output FILE"
      , Option ['t'] [] (ReqArg setThreads "N") "use N threads"
      ]
    setOutput out opts = opts { output = out }
    setThreads n opts = opts { threads = read n }

die :: String -> IO ()
die str = hPutStrLn stderr str >> exitFailure

getArchives :: Options -> IO (Map Text Package)
getArchives Options {..} =
  W.withSession $ \ses -> do
    archives <- runConcurrently $ for uris $ \uri ->
      Concurrently (getPackages ses uri)
    let pkgs = foldr (M.unionWith keepLatestVersion) M.empty archives
    oldPkgs <- readPackages output
    sem <- newQSem threads
    runConcurrently $ M.traverseWithKey (hashPackage oldPkgs sem ses) pkgs
  where
    keepLatestVersion a b =
      case comparing ver a b of
        LT -> b
        GT -> a
        EQ -> b

getPackages :: Session -> String -> IO (Map Text Package)
getPackages ses uri = do
  archive <- fetchArchive ses uri
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    B.hPutStr h archive
    hClose h
    M.map setArchive <$> readArchive path
  where
    setArchive pkg = pkg { archive = Just uri }

fetchArchive :: Session -> String -> IO ByteString
fetchArchive ses uri =
  view W.responseBody <$> W.get ses (uri </> "archive-contents")

readArchive :: FilePath -> IO (Map Text Package)
readArchive path = do
  load <- getDataFileName "elpa2json.el"
  json <- readProcess "emacs" ["--batch", "--load", load, "--eval", eval] ""
  let Just pkgs = JSON.decode $ BC.pack json
  return pkgs
  where
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"

readPackages :: FilePath -> IO (Map Text Package)
readPackages path =
  handle (\(SomeException _) -> return M.empty) $ do
    json <- B.readFile path
    let Just pkgs = JSON.decode json
    return pkgs

hashPackage :: Map Text Package -> QSem -> Session -> Text -> Package
            -> Concurrently Package
hashPackage pkgs sem ses name pkg =
  Concurrently $ handle brokenPkg $
  case M.lookup name pkgs of
    Just pkg' | isJust (hash pkg') -> return pkg' { desc = "" }
    _ -> do
      body <- getPackage sem ses name pkg
      hash_ <- sha256 body
      return pkg { hash = Just hash_, desc = "" }
  where
    nameS = T.unpack name
    brokenPkg (SomeException e) = do
      putStrLn $ "marking " ++ nameS ++ " broken due to exception:\n" ++ show e
      return pkg { broken = Just True }

getPackage :: QSem -> Session -> Text -> Package -> IO ByteString
getPackage sem ses name pkg = do
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
      pkgurl = uri </> filename <.> ext
  resp <- withQSem sem (W.get ses pkgurl)
  let decompress =
        case view (W.responseHeader "Content-Encoding") resp of
          "gzip" -> GZ.decompress
          "identity" -> id
          "" -> id
          (BC.unpack . B.fromStrict -> other) ->
            error ("unsupported content encoding " ++ other)
  return $ decompress (view W.responseBody resp)

writePackages :: FilePath -> Map Text Package -> IO ()
writePackages path pkgs = BC.writeFile path (JSON.encode pkgs)

withQSem :: QSem -> IO a -> IO a
withQSem qsem go = bracket_ (waitQSem qsem) (signalQSem qsem) go

sha256 :: ByteString -> IO Text
sha256 bytes = do
  hash <- digest SHA256 bytes
  return (T.pack (hash >>= toHex))
