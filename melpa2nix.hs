{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2016 Thomas Tuegel

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent ( getNumCapabilities, setNumCapabilities )
import Control.Monad ( join )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import Options.Applicative

import Distribution.Melpa

main :: IO ()
main = join (execParser (info (helper <*> parser) desc))
  where
    desc = fullDesc <> progDesc "Generate Nix expressions from MELPA recipes"

parser :: Parser (IO ())
parser =
  melpa2nix
  <$> (threads <|> pure 0)
  <*> melpa
  <*> stable
  <*> work
  <*> output
  <*> indexOnly
  <*> packages
  where
    threads = option auto (long "threads" <> short 't' <> metavar "N"
                          <> help "use N threads; default is number of CPUs")
    melpa = strOption (long "melpa" <> metavar "DIR"
                        <> help "path to MELPA repository")
    stable = switch (long "stable"
                      <> help "generate packages from MELPA Stable")
    work = strOption (long "work" <> metavar "DIR"
                      <> help "path to temporary workspace")
    output = strOption (long "output" <> short 'o' <> metavar "FILE"
                        <> help "dump MELPA data to FILE")
    indexOnly = flag False True
                (long "index-only"
                  <> help "don't update packages, only update the index file")
    packages = Set.fromList . map T.pack
                <$> many (strArgument
                          (metavar "PACKAGE" <> help "only work on PACKAGE"))

melpa2nix :: Int  -- ^ number of threads to use
          -> FilePath  -- ^ path to MELPA repository
          -> Bool      -- ^ generate packages from MELPA Stable
          -> FilePath  -- ^ temporary workspace
          -> FilePath  -- ^ dump MELPA recipes here
          -> Bool  -- ^ only generate the index
          -> Set Text
          -> IO ()
melpa2nix nthreads melpaDir stable workDir melpaOut indexOnly packages = do
  -- set number of threads before beginning
  if nthreads > 0
     then setNumCapabilities nthreads
     else getNumCapabilities >>= setNumCapabilities . (* 4)

  updateMelpa melpaDir stable workDir melpaOut indexOnly packages
