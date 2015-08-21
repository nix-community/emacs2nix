{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Exception ( SomeException(..), handle )
import Control.Monad (join, when)
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Parser ( json' )
import Data.Aeson.Types ( parseJSON, parseMaybe )
import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import qualified Data.Text as T
import Options.Applicative
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

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
      <*> packages
      where
        threads = option auto (long "threads" <> short 't' <> metavar "N"
                              <> help "use N threads; default is number of CPUs")
        melpa = strOption (long "melpa" <> metavar "DIR"
                           <> help "path to MELPA repository")
        work = strOption (long "work" <> metavar "DIR"
                          <> help "path to temporary workspace")
        output = strOption (long "output" <> short 'o' <> metavar "FILE"
                            <> help "dump MELPA data to FILE")
        packages = Set.fromList . map T.pack
                   <$> many (strArgument
                             (metavar "PACKAGE" <> help "only work on PACKAGE"))

melpa2nix :: Int  -- ^ number of threads to use
          -> FilePath  -- ^ path to MELPA repository
          -> FilePath  -- ^ temporary workspace
          -> FilePath  -- ^ dump MELPA recipes here
          -> Set Text
          -> IO ()
melpa2nix nthreads melpaDir workDir melpaOut packages = do
  when (nthreads > 0) $ setNumCapabilities nthreads

  oldPackages <- fromMaybe Map.empty <$> handle
    (\(SomeException _) -> return Nothing)
    (parseMaybe parseJSON <$> S.withFileAsInput melpaOut (S.parseFromStream json'))

  melpa <- getMelpa nthreads melpaDir workDir oldPackages packages

  S.withFileAsOutput melpaOut $ \out -> do
    enc <- S.fromLazyByteString (encodePretty melpa)
    S.connect enc out
