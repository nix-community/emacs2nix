module Main where

import Control.Concurrent.Async
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Traversable (traverse)
import GHC.Generics
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
  , dist :: Text
  , props :: Map Text Text
  , hash :: Maybe Text
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

fetchArchiveContents :: URI -> IO FilePath

readArchiveContents :: FilePath -> IO (Map Text Package)

readEncodedPackages :: FilePath -> IO (Map Text Package)

hashPackage :: Map Text Package -> URI -> Package -> IO Package

encodePackages :: Map Text Package -> IO ()
encodePackages = B.putStrLn . encode
