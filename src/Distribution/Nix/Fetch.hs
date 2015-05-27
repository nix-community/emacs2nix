{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Fetch where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions
  , genericParseJSON, genericToJSON )
import qualified Data.Char as Char
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.Environment (getEnvironment)
import qualified System.IO.Streams as S

data Fetch = URL { url :: Text, sha256 :: Maybe Text }
           | Git { url :: Text, rev :: Text, branchName :: Maybe Text, sha256 :: Maybe Text }
           | Bzr { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | CVS { url :: Text, cvsModule :: Maybe Text, sha256 :: Maybe Text }
           | Hg { url :: Text, rev :: Text, sha256 :: Maybe Text }
           | SVN { url :: Text, rev :: Text, sha256 :: Maybe Text }
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

prefetch :: Text -> Fetch -> IO (FilePath, Fetch)

prefetch _ fetch@(URL {..}) = do
  let args = [T.unpack url]
  oldEnv <- M.fromList <$> getEnvironment
  let env = M.toList (M.insert "PRINT_PATH" "1" oldEnv)
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-url" args Nothing (Just env)
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)

prefetch _ fetch@(Git {..}) = do
  let args = ["--url", T.unpack url, "--rev", T.unpack rev]
             ++ fromMaybe [] (do name <- branchName
                                 return ["--branch-name", T.unpack name])
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-git" args Nothing Nothing
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (_:hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)

prefetch _ fetch@(Bzr {..}) = do
  let args = [T.unpack url, T.unpack rev]
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-bzr" args Nothing Nothing
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)

prefetch _ fetch@(Hg {..}) = do
  let args = [T.unpack url, T.unpack rev]
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-hg" args Nothing Nothing
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)

prefetch name fetch@(CVS {..}) = do
  let args = [T.unpack url, T.unpack (fromMaybe name cvsModule)]
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-cvs" args Nothing Nothing
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)

prefetch _ fetch@(SVN {..}) = do
  let args = [T.unpack url, T.unpack rev]
  (_, out, err, pid) <- S.runInteractiveProcess "nix-prefetch-svn" args Nothing Nothing
  hashes <- S.lines out >>= S.decodeUtf8 >>= S.toList
  _ <- S.waitForProcess pid
  case hashes of
    (hash:path:_) -> return (T.unpack path, fetch { sha256 = Just hash })
    _ -> S.supply err S.stderr >> error ("unable to prefetch " ++ T.unpack url)
