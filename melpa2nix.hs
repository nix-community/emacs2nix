{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (setNumCapabilities)
import Control.Monad (filterM, join, when, unless)
import Data.List ( isPrefixOf )
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T ( encodeUtf8 )
import Options.Applicative
import System.Directory
       ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents )
import System.FilePath ((</>))
import qualified System.IO.Streams as S

import Distribution.Melpa
import qualified Distribution.Nix.Package.Melpa as Nix
import Distribution.Nix.Pretty hiding ((</>))

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
  when (nthreads > 0) $ setNumCapabilities nthreads

  unless indexOnly
    (updateMelpa nthreads melpaDir stable workDir melpaOut packages)

  updateIndex melpaOut

updateIndex :: FilePath -> IO ()
updateIndex output = do
  createDirectoryIfMissing True output

  let
    packageNamesOnly path
      | "." `isPrefixOf` path = pure False -- skip special files
      | otherwise = doesDirectoryExist (output </> path)
                    -- each packages is a directory
  contents <- getDirectoryContents output >>= filterM packageNamesOnly

  let
    pnames = map T.pack contents
    writeIndex out = do
      let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80)
                (pretty (Nix.packageSet pnames))
      encoded <- S.fromLazyByteString lbs
      S.connect encoded out
    file = output </> "default.nix"
  S.withFileAsOutput file writeIndex
