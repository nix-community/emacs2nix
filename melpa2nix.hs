{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent ( setNumCapabilities )
import Control.Monad ( join, when, unless )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import Options.Applicative

import Distribution.Melpa
import Distribution.Nix.Index

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
  when (nthreads > 0) $ setNumCapabilities nthreads

  unless indexOnly (updateMelpa melpaDir stable workDir melpaOut packages)

  updateIndex melpaOut
