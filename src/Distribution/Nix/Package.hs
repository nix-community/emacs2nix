{-# LANGUAGE DeriveGeneric #-}

module Distribution.Nix.Package where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import Data.Text (Text)
import GHC.Generics

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

data Build = MelpaPackage { recipe :: Text }
           | ElpaPackage
           deriving Generic

buildOptions :: Options
buildOptions = defaultOptions
               { constructorTagModifier = buildTagModifier
               , sumEncoding = ObjectWithSingleField
               }
  where
    buildTagModifier [] = []
    buildTagModifier (x:xs) = Char.toLower x : xs

instance FromJSON Build where
  parseJSON = genericParseJSON buildOptions

instance ToJSON Build where
  toJSON = genericToJSON buildOptions

data Package = Package
               { ver :: Text
               , deps :: [Text]
               , fetch :: Fetch
               , build :: Build
               }
             deriving Generic

packageOptions :: Options
packageOptions = defaultOptions { omitNothingFields = True }

instance FromJSON Package where
  parseJSON = genericParseJSON packageOptions

instance ToJSON Package where
  toJSON = genericToJSON packageOptions
