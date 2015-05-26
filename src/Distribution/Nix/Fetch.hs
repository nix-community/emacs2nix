{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import qualified System.IO.Streams as S

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { url :: Text, cvsModule :: Text, sha256 :: Maybe Text }
           | Hg { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | SVN { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | NoFetch
           deriving Generic

fetchOptions :: Options
fetchOptions = defaultOptions
               { constructorTagModifier = ("fetch" ++) . map Char.toLower
               , sumEncoding = ObjectWithSingleField
               , omitNothingFields = True
               , fieldLabelModifier = fetchLabelModifier
               }
  where
    fetchLabelModifier field =
      case field of
        "cvsModule" -> "module"
        _ -> field

instance FromJSON Fetch where
  parseJSON = genericParseJSON fetchOptions

instance ToJSON Fetch where
  toJSON = genericToJSON fetchOptions

prefetch :: Fetch -> IO Fetch
prefetch fetch@(URL {..}) = do
  let args = [T.unpack url]
  (inp, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-url" args Nothing Nothing
  S.write Nothing inp
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    [] -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)
    (hash:_) -> return fetch { sha256 = Just hash }
