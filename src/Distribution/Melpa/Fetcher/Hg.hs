{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.Hg ( Hg, fetchHg ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Hg =
  Hg
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Hg where
  toJSON = wrapFetcher "hg" . genericToJSON defaultOptions

instance FromJSON Hg where
  parseJSON = genericParseJSON defaultOptions

fetchHg :: Fetcher Hg
fetchHg = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let Hg _hg@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (hgEnv name _hg)
  _hg <- return _hg { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (hgEnv name _hg)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Hg _hg }
    , Package.hash = _hash
    }

hgEnv :: Text -> Hg -> HashMap Text Text
hgEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "hg"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
-}
