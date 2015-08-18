{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Monad (join, when)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative
import qualified System.IO.Streams as S

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
      <*> work
      <*> output
      where
        threads = option auto (long "threads" <> short 't' <> metavar "N"
                              <> help "use N threads; default is number of CPUs")
        melpa = strOption (long "melpa" <> metavar "DIR"
                           <> help "path to MELPA repository")
        work = strOption (long "work" <> metavar "DIR"
                          <> help "path to temporary workspace")
        output = strOption (long "output" <> short 'o' <> metavar "FILE"
                            <> help "dump MELPA data to FILE")

melpa2nix :: Int  -- ^ number of threads to use
          -> FilePath  -- ^ path to MELPA repository
          -> FilePath  -- ^ temporary workspace
          -> FilePath  -- ^ dump MELPA recipes here
          -> IO ()
melpa2nix nthreads melpaDir workDir melpaOut = do
  when (nthreads > 0) $ setNumCapabilities nthreads

  melpa <- getMelpa nthreads melpaDir workDir

  S.withFileAsOutput melpaOut $ \out -> do
    enc <- S.fromLazyByteString (encodePretty melpa)
    S.connect enc out
