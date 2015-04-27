{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [melpa, nixpkgs] -> main_go melpa nixpkgs
    _ -> T.putStrLn usage
  where
    main_go melpa nixpkgs = return ()

usage :: Text
usage =
  "USAGE: melpa2nix MELPA NIXPKGS\n\
  \  MELPA    location of MELPA repo\n\
  \  NIXPKGS  location of Nixpkgs repo to update"
