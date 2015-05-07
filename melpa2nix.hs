{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified System.IO.Streams as S

import Distribution.Melpa

main :: IO ()
main = do
  args <- getArgs
  case args of
    [melpa, nixpkgs] -> main_go melpa nixpkgs True
    _ -> T.putStrLn usage
  where
    main_go melpa nixpkgs stable = do
      pkgs <- updateMelpa melpa nixpkgs stable
      encoded <- S.fromLazyByteString (encodePretty pkgs)
      let filename | stable = (nixpkgs </> "pkgs/top-level/emacs-packages.json")
                   | otherwise = (nixpkgs </> "pkgs/top-level/emacs-packages-unstable.json")
      S.withFileAsOutput filename (S.connect encoded)

usage :: Text
usage =
  "USAGE: melpa2nix MELPA NIXPKGS\n\
  \  MELPA    location of MELPA repo\n\
  \  NIXPKGS  location of Nixpkgs repo to update"
