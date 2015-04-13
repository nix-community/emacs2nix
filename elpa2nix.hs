module Main where

import Control.Concurrent.Async
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable (traverse)
import GHC.Generics
import Network.HTTP.Client
import Network.URI (URI, parseURI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= validateURIs >>= generatePackagesFromArchives >>= encodePackages

data Package =
  Package
  { ver :: [Int]
  , deps :: Map Text [Int]
  , desc :: Text
  , dist :: Text -- TODO: replace with an enumeration
  , props :: Map Text Text
  , hash :: Maybe ByteString
  , archive :: Maybe String
  }
  deriving (Eq, Generic)

instance FromJSON Package where
  parseJSON = gParseJSON

instance ToJSON Package where
  toJSON = gToJSON

validateURIs :: [String] -> IO [URI]
validateURIs = traverse $ \str ->
  case parseURI str of
    Nothing -> die ("Invalid URI: " ++ str)
    Just uri -> return uri

die :: String -> IO ()
die str = hPutStrLn stderr str >> exitFailure

generatePackagesFromArchives :: [URI] -> Map Text Package

fetchArchiveContents :: URI -> IO ByteString

readArchiveContents :: FilePath -> IO (Map Text Package)

readEncodedPackages :: FilePath -> IO (Map Text Package)
readEncodedPackages = decode . B.readFile

hashPackage :: Map Text Package -> Manager -> URI -> Text -> Package -> IO Package

encodePackages :: Map Text Package -> IO ()
encodePackages = B.putStrLn . encode
