{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa.Fetcher where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Distribution.Melpa.Fetcher.Bzr.Types (Bzr)
import Distribution.Melpa.Fetcher.CVS.Types (CVS)
import Distribution.Melpa.Fetcher.Darcs.Types (Darcs)
import Distribution.Melpa.Fetcher.Fossil.Types (Fossil)
import Distribution.Melpa.Fetcher.Git.Types (Git)
import Distribution.Melpa.Fetcher.GitHub.Types (GitHub)
import Distribution.Melpa.Fetcher.Hg.Types (Hg)
import Distribution.Melpa.Fetcher.SVN.Types (SVN)
import Distribution.Melpa.Fetcher.Wiki.Types (Wiki)

data Fetcher
  = Git !Git
  | GitHub !GitHub
  | Bzr !Bzr
  | Hg !Hg
  | Darcs !Darcs
  | Fossil !Fossil
  | SVN !SVN
  | CVS !CVS
  | Wiki !Wiki
  deriving (Eq, Read, Show)

instance FromJSON Fetcher where
  parseJSON = withObject "fetcher" $ \obj -> do
    fetch <- obj .: "fetcher"
    let obj_ = Object obj
    case fetch of
      "git" -> Git <$> parseJSON obj_
      "github" -> GitHub <$> parseJSON obj_
      "bzr" -> Bzr <$> parseJSON obj_
      "hg" -> Hg <$> parseJSON obj_
      "darcs" -> Darcs <$> parseJSON obj_
      "fossil" -> Fossil <$> parseJSON obj_
      "svn" -> SVN <$> parseJSON obj_
      "cvs" -> CVS <$> parseJSON obj_
      "wiki" -> Wiki <$> parseJSON obj_
      unknown -> fail ("unknown fetcher '" ++ T.unpack unknown ++ "'")

instance ToJSON Fetcher where
  toJSON (Git git) = wrapFetcher "git" (toJSON git)
  toJSON (GitHub github) = wrapFetcher "github" (toJSON github)
  toJSON (Bzr bzr) = wrapFetcher "bzr" (toJSON bzr)
  toJSON (Hg hg) = wrapFetcher "hg" (toJSON hg)
  toJSON (Darcs darcs) = wrapFetcher "darcs" (toJSON darcs)
  toJSON (Fossil fossil) = wrapFetcher "fossil" (toJSON fossil)
  toJSON (SVN svn) = wrapFetcher "svn" (toJSON svn)
  toJSON (CVS cvs) = wrapFetcher "cvs" (toJSON cvs)
  toJSON (Wiki wiki) = wrapFetcher "wiki" (toJSON wiki)

wrapFetcher :: Text -> Value -> Value
wrapFetcher fetch val =
  case val of
    (Object obj) -> Object (HM.insert "fetcher" (toJSON fetch) obj)
    _ -> error "wrapFetcher: not a fetcher object!"
