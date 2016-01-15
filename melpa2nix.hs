{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Monad (join, when)
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T ( encodeUtf8 )
import Options.Applicative
import qualified System.IO.Streams as S

import Distribution.Melpa
import qualified Distribution.Nix.Package.Melpa as Nix
import Distribution.Nix.Pretty

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
        packages = Set.fromList . map T.pack
                   <$> many (strArgument
                             (metavar "PACKAGE" <> help "only work on PACKAGE"))

melpa2nix :: Int  -- ^ number of threads to use
          -> FilePath  -- ^ path to MELPA repository
          -> Bool      -- ^ generate packages from MELPA Stable
          -> FilePath  -- ^ temporary workspace
          -> FilePath  -- ^ dump MELPA recipes here
          -> Set Text
          -> IO ()
melpa2nix nthreads melpaDir stable workDir melpaOut packages = do
  when (nthreads > 0) $ setNumCapabilities nthreads

  melpa <- getMelpa nthreads melpaDir stable workDir packages

  S.withFileAsOutput melpaOut $ \out -> do
    let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80) (Nix.packageSet melpa)
    enc <- S.fromLazyByteString lbs
    S.connect enc out
