{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Melpa.Fetcher.Bzr ( Bzr, fetchBzr ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Error
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics

import Distribution.Melpa.Fetcher

data Bzr =
  Bzr
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

type instance Rev Bzr = Text

instance ToJSON Bzr where
  toJSON = wrapFetcher "bzr" . genericToJSON defaultOptions

instance FromJSON Bzr where
  parseJSON = genericParseJSON defaultOptions

fetchBzr :: Fetcher Bzr
fetchBzr = undefined

{-
hash :: FilePath -> FilePath -> Bool -> Text -> Archive -> Recipe -> EitherT Text IO Package
hash _ _ True name _ _ = left (name <> ": stable fetcher 'bzr' not implemented")
hash melpa _ stable name arch rcp = do
  let Bzr _bzr@(Fetcher {..}) = fetcher rcp
  _commit <- getCommit melpa stable name _bzr
  _hash <- prefetch name _bzr _commit
  return Package
    { P.ver = A.ver arch
    , P.deps = maybe [] HM.keys (A.deps arch)
    , P.recipe = rcp { fetcher = Bzr (_bzr { commit = Just _commit }) }
    , P.hash = _hash
    }

getCommit :: FilePath -> Bool -> Text -> Bzr -> EitherT Text IO Text
getCommit melpa stable name Fetcher {..} =
  left (name <> ": fetcher 'bzr' not implemented")
  {-
  if stable
    then left (name <> ": stable fetcher 'bzr' not implemented")
  else do
    let revno = "revno:" *> spaces *> plus digit
    mcommit <- liftIO
               $ indir (melpa </> "working" </> fromString (T.unpack name))
               $ fold (grep revno (inproc "bzr" ["log"] empty)) F.head
    case mcommit of
      Nothing -> left (name <> ": could not get bzr revno")
      Just c -> return c
  -}

prefetch :: Text -> Bzr -> Text -> EitherT Text IO Text
prefetch name bzr commit =
  left (name <> ": fetcher 'bzr' not implemented")
  {-
  mhash <- liftIO $ do
    export "QUIET" "1"
    mhash <- fold (inproc "nix-prefetch-bzr" [url bzr, commit] empty) F.last
    unset "QUIET"
    return mhash
  case mhash of
    Nothing -> left (name <> ": could not prefetch bzr")
    Just h -> return h
  -}
-}
