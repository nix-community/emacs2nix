{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Fetcher.SVN ( SVN, fetchSVN ) where

import Control.Applicative
import Control.Error
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Attoparsec.ByteString.Char8
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Prelude hiding (takeWhile)
import qualified System.IO.Streams as S

import Distribution.Melpa.Fetcher

data SVN =
  SVN
  { url :: Text
  , commit :: Maybe Text
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON SVN where
  toJSON = wrapFetcher "svn" . genericToJSON defaultOptions

instance FromJSON SVN where
  parseJSON = genericParseJSON defaultOptions

fetchSVN :: Fetcher SVN
fetchSVN = Fetcher {..}
  where
    getRev _ SVN {..} tmp =
      handleAll $ EitherT $ bracket
        (S.runInteractiveProcess "svn" ["info"] (Just tmp) Nothing)
        (\(_, _, _, pid) -> S.waitForProcess pid)
        (\(inp, out, _, _) -> do
               S.write Nothing inp
               lines_ <- S.lines out >>= S.decodeUtf8 >>= S.toList
               let revs = catMaybes (svnRev <$> lines_)
               return $ headErr "could not find revision" revs)

    prefetch name SVN {..} rev =
      prefetchWith name "nix-prefetch-svn" [ T.unpack url, T.unpack rev ]

-- | Read a line from @svn info@ and return the revision if the line begins with \"Revision:\".
svnRev :: Text -> Maybe Text
svnRev = fmap T.strip . T.stripPrefix "Revision:"
