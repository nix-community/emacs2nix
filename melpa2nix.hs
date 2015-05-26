{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Monad (join, when)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text as T
import Options.Applicative
import qualified System.IO.Streams as S

import Distribution.Melpa

main :: IO ()
main = join $ execParser
       (info (helper <*> melpa2nixParser)
        (fullDesc <> progDesc "Generate Nix expressions from MELPA recipes"))

melpa2nixParser :: Parser (IO ())
melpa2nixParser =
  melpa2nix

  <$> (threads <|> pure 0)

  <*> strOption (long "melpa-dir" <> metavar "DIR"
                 <> help "path to MELPA repository")

  <*> strOption (long "work-dir" <> metavar "DIR"
                 <> help "path to temporary workspace")

  <*> strOption (long "melpa-out" <> metavar "FILE"
                 <> help "dump MELPA data to FILE")
  where
    threads = option auto (long "threads" <> metavar "N"
                           <> help "use N threads; default is number of CPUs")

melpa2nix :: Int  -- ^ number of threads to use
          -> FilePath  -- ^ path to MELPA repository
          -> FilePath  -- ^ temporary workspace
          -> FilePath  -- ^ dump MELPA recipes here
          -> IO ()
melpa2nix nthreads melpaDir workDir melpaOut = do
  when (nthreads > 0) $ setNumCapabilities nthreads

  oldMelpa <- readMelpa melpaOut

  melpa <- getMelpa melpaDir workDir oldMelpa >>= \case
    Left errmsg -> fail (T.unpack errmsg)
    Right result -> return result

  S.withFileAsOutput melpaOut $ \out -> do
    enc <- S.fromLazyByteString (encodePretty melpa)
    S.connect enc out
