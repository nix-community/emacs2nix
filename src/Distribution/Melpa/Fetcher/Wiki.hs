{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.Wiki ( Wiki, fetchWiki ) where

import Control.Error hiding (runScript)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Wiki =
  Wiki
  { url :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev Wiki = ()

instance ToJSON Wiki where
  toJSON = wrapFetcher "wiki" . genericToJSON defaultOptions

instance FromJSON Wiki where
  parseJSON = genericParseJSON defaultOptions

fetchWiki :: Fetcher Wiki
fetchWiki = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ nixpkgs _ name arch rcp = do
  let Wiki _wiki@(Fetcher {..}) = fetcher rcp
  _hash <- prefetch nixpkgs name Nothing (wikiEnv name _wiki)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp
    , Package.hash = _hash
    }

wikiEnv :: Text -> Wiki -> HashMap Text Text
wikiEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "wiki"), ("name", name) ]
  ++ maybeToList ((,) "url" <$> url)
-}
