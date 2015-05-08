{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Prelude hiding (FilePath)
import System.Environment (getArgs)
import qualified System.IO.Streams as S
import Turtle

import Distribution.Melpa

main :: IO ()
main = do
  args <- getArgs
  case args of
    [decodeString -> melpa, decodeString -> nixpkgs] ->
      main_go melpa nixpkgs True
    _ -> T.putStrLn usage
  where
    main_go melpa nixpkgs stable = do
      pkgs <- updateMelpa melpa nixpkgs stable
      encoded <- S.fromLazyByteString (encodePretty pkgs)
      let outfile | stable = (nixpkgs </> "pkgs/top-level/emacs-packages.json")
                  | otherwise = (nixpkgs </> "pkgs/top-level/emacs-packages-unstable.json")
      S.withFileAsOutput (encodeString outfile) (S.connect encoded)

usage :: Text
usage =
  "USAGE: melpa2nix MELPA NIXPKGS\n\
  \  MELPA    location of MELPA repo\n\
  \  NIXPKGS  location of Nixpkgs repo to update"
