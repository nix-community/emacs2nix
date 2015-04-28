{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified System.IO.Streams as S

main :: IO ()
main = do
  args <- getArgs
  case args of
    [melpa, nixpkgs] -> main_go melpa nixpkgs
    _ -> T.putStrLn usage
  where
    main_go melpa nixpkgs = do
      stable <- updateMelpaStable melpa
      unstable <- updateMelpa melpa
      let packages = HM.union stable unstable -- prefers stable
      encoded <- S.fromLazyByteString (encodePretty packages)
      let filename = (nixpkgs </> "pkgs/top-level/emacs-packages.json")
      S.withFileAsOutput filename (S.connect encoded)

usage :: Text
usage =
  "USAGE: melpa2nix MELPA NIXPKGS\n\
  \  MELPA    location of MELPA repo\n\
  \  NIXPKGS  location of Nixpkgs repo to update"
