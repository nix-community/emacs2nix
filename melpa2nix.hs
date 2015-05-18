{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (join)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Options.Applicative
import qualified System.IO.Streams as S

import Distribution.Melpa.Package
import Distribution.Melpa.Recipe

main :: IO ()
main = join $ execParser
       (info (helper <*> melpa2nixParser)
        (fullDesc <> progDesc "Generate Nix expressions from MELPA recipes"))

melpa2nixParser :: Parser (IO ())
melpa2nixParser =
  melpa2nix

  <$> strOption (long "package-build" <> metavar "FILE"
                 <> help "path to package-build.el")

  <*> strOption (long "recipes-dir" <> metavar "DIR"
                 <> help "path to MELPA recipes")

  <*> strOption (long "recipes-out" <> metavar "FILE"
                 <> help "dump MELPA recipes to FILE")

  <*> strOption (long "packages-out" <> metavar "FILE"
                 <> help "dump packages to FILE")

melpa2nix :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
melpa2nix packageBuild recipesDir recipesOut packagesOut = do
  dumpRecipes packageBuild recipesDir recipesOut
  recipes <- readRecipes packageBuild recipesOut
  let getPackage_ = getPackage packageBuild recipesOut
  (errors, packages) <- partitionEithers . map liftEither . M.toList
                        <$> M.traverseWithKey getPackage_ recipes
  for_ errors $ \(name, err) -> T.putStrLn (name <> ": " <> err)
  S.withFileAsOutput packagesOut $ \out -> do
    enc <- S.fromLazyByteString (encodePretty $ M.fromList packages)
    S.connect enc out
  where
    liftEither (name, stat) = either (Left . (,) name) (Right . (,) name) stat
