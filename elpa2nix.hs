module Main where

import Control.Concurrent.Async
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Network.URI (parseURI)

main :: IO ()
main = getArgs >>= validateURIs >>= generatePackagesFromArchives >>= encodePackages

validateURIs :: [String] -> IO [URI]
validateURIs = traverse $ \str ->
  case parseURI str of
    Nothing -> die ("Invalid URI: " ++ str)
    Just uri -> return uri

die :: String -> IO ()
die str = hPutStrLn stderr str >> exitFailure

generatePackagesFromArchives :: [URI] -> Map Text HashedPackage

fetchArchiveContents :: URI -> IO FilePath

readArchiveContents :: FilePath -> IO (Map Text Package)

hashPackage :: URI -> Package -> IO HashedPackage

encodePackages :: Map Text HashedPackage -> IO ()
encodePackages = B.putStrLn . encode
