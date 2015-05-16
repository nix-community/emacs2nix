{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.SVN ( SVN, fetchSVN ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data SVN =
  SVN
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev SVN = Text

instance ToJSON SVN where
  toJSON = wrapFetcher "svn" . genericToJSON defaultOptions

instance FromJSON SVN where
  parseJSON = genericParseJSON defaultOptions

fetchSVN :: Fetcher SVN
fetchSVN = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let SVN _svn@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (svnEnv name _svn)
  _svn <- return _svn { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (svnEnv name _svn)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = SVN _svn }
    , Package.hash = _hash
    }

svnEnv :: Text -> SVN -> HashMap Text Text
svnEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "svn"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
-}
