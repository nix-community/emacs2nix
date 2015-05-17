{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (isRight)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Options.Applicative
import qualified System.IO.Streams as S

import Distribution.Melpa.Package
import Distribution.Melpa.Recipe

data Melpa2nix =
  Melpa2nix
  { packageBuild :: FilePath
  , recipesDir :: FilePath
  , recipesOut :: FilePath
  , packagesOut :: FilePath
  }

melpa2nixParser :: Parser Melpa2nix
melpa2nixParser =
  Melpa2nix
  <$> strOption (long "package-build" <> metavar "FILE" <> help "path to package-build.el")
  <*> strOption (long "recipes-dir" <> metavar "DIR" <> help "path to MELPA recipes")
  <*> strOption (long "recipes-out" <> metavar "FILE" <> help "dump MELPA recipes to FILE")
  <*> strOption (long "packages-out" <> metavar "FILE" <> help "dump packages to FILE")

main :: IO ()
main = execParser opts >>= melpa2nix
  where
    opts = info
           (helper <*> melpa2nixParser)
           (fullDesc <> progDesc "Generate Nix expressions from MELPA recipes")

melpa2nix :: Melpa2nix -> IO ()
melpa2nix Melpa2nix {..} = do
  dumpRecipes packageBuild recipesDir recipesOut
  recipes <- readRecipes packageBuild recipesOut
  epackages <- M.traverseWithKey (getPackage packageBuild recipesOut) recipes
  for_ epackages $ \epkg ->
    case epkg of
      Left err -> T.putStrLn err
      Right _ -> return ()
  let packages = M.map fromRight (M.filter isRight epackages)
  S.withFileAsOutput packagesOut $ \out -> do
    enc <- S.fromLazyByteString (encodePretty packages)
    S.connect enc out
  where
    fromRight (Right x) = x
    fromRight _ = error "fromRight: the impossible happened!"
