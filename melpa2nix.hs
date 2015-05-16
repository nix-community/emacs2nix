{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Options.Applicative
import System.Environment (getArgs)
import System.FilePath
import qualified System.IO.Streams as S

import Distribution.Melpa

data Melpa2nix =
  Melpa2nix
  { packageBuild :: FilePath
  , recipes :: FilePath
  }

melpa2nixParser :: Parser Melpa2nix
melpa2nixParser =
  Melpa2nix
  <$> strOption (long "package-build" <> metavar "FILE" <> help "path to package-build.el")
  <*> strOption (long "recipes" <> metavar "DIR" <> help "path to MELPA recipes")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [melpa, nixpkgs] ->
      main_go melpa nixpkgs True
    _ -> T.putStrLn usage
  where
    main_go melpa nixpkgs stable = do
      pkgs <- updateMelpa melpa nixpkgs stable
      encoded <- S.fromLazyByteString (encodePretty pkgs)
      let outfile | stable = (nixpkgs </> "pkgs/top-level/emacs-packages.json")
                  | otherwise = (nixpkgs </> "pkgs/top-level/emacs-packages-unstable.json")
      S.withFileAsOutput outfile (S.connect encoded)

usage :: Text
usage =
  "USAGE: melpa2nix MELPA NIXPKGS\n\
  \  MELPA    location of MELPA repo\n\
  \  NIXPKGS  location of Nixpkgs repo to update"
