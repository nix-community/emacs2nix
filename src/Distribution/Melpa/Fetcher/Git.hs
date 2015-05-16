{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.Git ( Git, fetchGit ) where

import Control.Error hiding (runScript)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Git =
  Git
  { url :: Text
  , commit :: Maybe Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev Git = Text

instance ToJSON Git where
  toJSON = wrapFetcher "git" . genericToJSON defaultOptions

instance FromJSON Git where
  parseJSON = genericParseJSON defaultOptions

fetchGit :: Fetcher Git
fetchGit = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash melpa nixpkgs stable name arch rcp = do
  let Git _git@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name Nothing (gitEnv name _git)
  _git <- return _git { commit = Just _commit }
  _hash <- prefetch nixpkgs name Nothing (gitEnv name _git)
  return Package
    { Package.ver = Archive.ver arch
    , Package.deps = maybe [] HM.keys (Archive.deps arch)
    , Package.recipe = rcp { fetcher = Git _git }
    , Package.hash = _hash
    }

gitEnv :: Text -> Git -> HashMap Text Text
gitEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "git"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "commit" <$> commit)
  ++ maybeToList ((,) "branch" <$> branch)
-}
