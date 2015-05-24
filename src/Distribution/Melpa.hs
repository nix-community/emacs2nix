{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa (Melpa(..), readMelpa) where

import Control.Error
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.FilePath
import qualified System.IO.Streams as S

import Distribution.Melpa.Recipe

data Melpa = Melpa
             { rev :: Text
             , recipeHashes :: Map Text Text
             }
           deriving Generic

instance ToJSON Melpa where
  toJSON = genericToJSON defaultOptions

instance FromJSON Melpa where
  parseJSON = genericParseJSON defaultOptions

readMelpa :: FilePath -> IO (Either Text (Melpa, Map Text Recipe))
readMelpa melpaDir = runEitherT $ do
  rev <- getRev_Melpa melpaDir
  recipes <- liftIO (readRecipes melpaDir)
  recipeHashes <- M.fromList <$> traverse (getRecipeHash melpaDir) (M.keys recipes)
  return (Melpa {..}, recipes)

getRev_Melpa :: FilePath -> EitherT Text IO Text
getRev_Melpa melpaDir = EitherT $ do
  let args = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
  bracket
    (S.runInteractiveProcess "git" args (Just melpaDir) Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
         S.write Nothing inp
         revs <- S.toList =<< S.decodeUtf8 =<< S.lines out
         return (headErr "could not get revision" revs))

getRecipeHash :: FilePath -> Text -> EitherT Text IO (Text, Text)
getRecipeHash melpaDir name = EitherT $ do
  let args = [ "--base32", "--type", "sha256", melpaDir </> "recipes" </> T.unpack name ]
  bracket
    (S.runInteractiveProcess "nix-hash" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(inp, out, _, _) -> do
         S.write Nothing inp
         sha256s <- S.toList =<< S.decodeUtf8 =<< S.lines out
         return ((,) name <$> headErr "could not calculate hash" sha256s))
