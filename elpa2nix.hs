{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception (SomeException(..), bracket_, handle)
import Crypto.Hash (Digest, SHA256, digestToHexByteString, hashlazy)
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
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import GHC.Generics
import Network.HTTP.Client
  ( Manager, Request, decompress, httpLbs, managerModifyRequest, parseUrl
  , responseBody, withManager )
import Network.HTTP.Client.TLS (tlsManagerSettings)
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
  , input :: FilePath
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
    , input = if input opts == "" then output opts else input opts
    }
  where
    header = "Usage: elpa2nix [OPTION...] URIs..."
    defaultOptions = Options { output = "", uris = [], threads = 0 }
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
getArchives Options {..} =
  withManager mgrSettings $ \ses -> do
    archives <- runConcurrently $ for uris $ \uri ->
      Concurrently (getPackages ses uri)
    oldPkgs <- readPackages input
    let pkgs = foldr (M.unionWith keepLatestVersion) oldPkgs archives
    sem <- newQSem threads
    runConcurrently $ M.traverseWithKey (hashPackage oldPkgs sem ses) pkgs
  where
    keepLatestVersion a b =
      case comparing ver a b of
        LT -> b
        GT -> a
        EQ -> b
    mgrSettings =
      tlsManagerSettings
      { managerModifyRequest = \req ->
            alwaysDecompress <$> managerModifyRequest tlsManagerSettings req
      }

getPackages :: Manager -> String -> IO (Map Text Package)
getPackages man uri = do
  archive <- fetchArchive man uri
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    B.hPutStr h archive
    hClose h
    M.map setArchive <$> readArchive path
  where
    setArchive pkg = pkg { archive = Just uri }

fetchArchive :: Manager -> String -> IO ByteString
fetchArchive man uri = do
  req <- alwaysDecompress <$> parseUrl (uri </> "archive-contents")
  responseBody <$> httpLbs req man

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

hashPackage :: Map Text Package -> QSem -> Manager -> Text -> Package
            -> Concurrently Package
hashPackage pkgs sem man name pkg =
  Concurrently $ handle brokenPkg $
  case M.lookup name pkgs of
    Just pkg' | isJust (hash pkg') -> return pkg' { desc = "" }
    _ -> do
      body <- fetchPackage sem man name pkg
      return pkg { hash = Just (sha256 body), desc = "" }
  where
    nameS = T.unpack name
    brokenPkg (SomeException e) = do
      putStrLn $ "marking " ++ nameS ++ " broken due to exception:\n" ++ show e
      return pkg { broken = Just True }

fetchPackage :: QSem -> Manager -> Text -> Package -> IO ByteString
fetchPackage sem man name pkg = do
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
  req <- alwaysDecompress <$> parseUrl (uri </> filename <.> ext)
  withQSem sem (responseBody <$> httpLbs req man)

writePackages :: FilePath -> Map Text Package -> IO ()
writePackages path pkgs = BC.writeFile path (JSON.encode pkgs)

withQSem :: QSem -> IO a -> IO a
withQSem qsem go = bracket_ (waitQSem qsem) (signalQSem qsem) go

sha256 :: ByteString -> Text
sha256 bytes =
  let dig :: Digest SHA256
      dig = hashlazy bytes
  in T.decodeUtf8 (digestToHexByteString dig)

alwaysDecompress :: Request -> Request
alwaysDecompress req = req { decompress = \_ -> True }
