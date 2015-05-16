{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.CVS ( CVS, fetchCVS ) where

import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data CVS =
  CVS
  { url :: Text
  , branch :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON CVS where
  toJSON = wrapFetcher "cvs" . renameFields . genericToJSON defaultOptions
    where renameFields (Object obj) =
            let renameFields_go ("branch", b) = ("module", b)
                renameFields_go other = other
            in Object (HM.fromList (map renameFields_go (HM.toList obj)))
          renameFields other = other

instance FromJSON CVS where
  parseJSON = genericParseJSON defaultOptions . renameFields
    where renameFields (Object obj) =
            let renameFields_go ("module", b) = ("branch", b)
                renameFields_go other = other
            in Object (HM.fromList (map renameFields_go (HM.toList obj)))
          renameFields other = other

fetchCVS :: Fetcher CVS
fetchCVS = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe
     -> EitherT Text IO Package
hash _ _ True name _ _ = left (name <> ": stable fetcher 'cvs' not implemented")
hash _ nixpkgs False name arch rcp = do
  let CVS _cvs@(Fetcher {..}) = fetcher rcp
  _hash <- prefetch nixpkgs name Nothing (cvsEnv name _cvs)
  return Package
    { P.ver = A.ver arch
    , P.deps = maybe [] HM.keys (A.deps arch)
    , P.recipe = rcp { fetcher = CVS _cvs }
    , P.hash = _hash
    }

cvsEnv :: Text -> CVS -> HashMap Text Text
cvsEnv name Fetcher {..} =
  HM.fromList
  $ [ ("fetcher", "cvs"), ("name", name), ("url", url) ]
  ++ maybeToList ((,) "branch" <$> branch)

-}
