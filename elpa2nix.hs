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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Arrow ( (>>>) )
import Control.Concurrent ( setNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Exception
import Control.Monad ( join, when )
import Data.Aeson ( FromJSON(..), json' )
import Data.Aeson.Types ( parseEither )
import Data.ByteString ( ByteString )
import Data.Function ( (&) )
import Data.HashMap.Strict ( HashMap )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Maybe ( catMaybes )
import Data.Monoid ((<>))
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import Nix.Expr ( NExpr )
import Options.Applicative
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose )
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp ( withSystemTempFile )

import Paths_emacs2nix

import Distribution.Elpa ( Elpa )
import qualified Distribution.Elpa as Elpa
import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Nix.Fetch as Nix
import Distribution.Nix.Index
import Distribution.Nix.Name ( Name )
import qualified Distribution.Nix.Name as Nix
import Distribution.Nix.Package.Elpa ( Package )
import qualified Distribution.Nix.Package.Elpa as Nix
import Exceptions
import Process

main :: IO ()
main = join (execParser (info (helper <*> parser) desc))
  where
    desc = fullDesc <> progDesc "Generate Nix expressions from ELPA"

parser :: Parser (IO ())
parser =
  elpa2nix
  <$> (threads <|> pure 0)
  <*> output
  <*> server
  <*> names
  <*> indexOnly
  where
    threads = option auto
              (long "threads" <> short 't'
               <> metavar "N"
               <> help "use N threads; default is number of CPUs")
    output = strOption
             (long "output" <> short 'o'
              <> metavar "FILE"
              <> help "write output to FILE")
    server = strArgument
             (metavar "URL"
              <> help "get packages from server at URL")
    names = strOption
            (long "names"
             <> metavar "FILE"
             <> help "map Emacs package names to Nix package names using FILE")
    indexOnly = flag False True
                (long "index-only"
                 <> help "don't update packages, only update the index file")

elpa2nix :: Int -> FilePath -> String -> FilePath -> Bool -> IO ()
elpa2nix threads output server namesMapFile indexOnly =
  catchPretty_ $ do
    when (threads > 0) (setNumCapabilities threads)

    namesMap <- Nix.readNames namesMapFile
    archives <- getPackages server

    updated <- if indexOnly
              then pure []
              else runConcurrently (traverse (updatePackage server namesMap) (M.toList archives))

    existing <- readIndex output

    let packages = M.union (M.fromList (catMaybes updated)) existing

    writeIndex output packages

updatePackage :: String -> HashMap Emacs.Name Name -> (Text, Elpa)
              -> Concurrently (Maybe (Name, NExpr))
updatePackage server namesMap elpa = Concurrently $ do
  hashed <- hashPackage server namesMap elpa
  pure (toExpression <$> hashed)
  where
    toExpression pkg = (Nix.pname pkg, Nix.expression pkg server)

-- * Error types

newtype ArchiveError = ArchiveError SomeException
  deriving (Show, Typeable)

instance Exception ArchiveError

-- * getPackages

getPackages :: String -> IO (Map Text Elpa)
getPackages uri = mapException ArchiveError $ do
  let args = [uri </> "archive-contents"]
  withSystemTempFile "elpa2nix-archive-contents-" $ \path h -> do
    runInteractiveProcess "curl" args Nothing Nothing $ \out -> do
      tmp <- S.handleToOutputStream h >>= S.atEndOfOutput (hClose h)
      S.connect out tmp
    readArchive path

-- * readArchive

type InputByteStream = S.InputStream ByteString

data ParseArchiveError = ParseArchiveError String
  deriving (Show, Typeable)

instance Exception ParseArchiveError

readArchive :: FilePath -> IO (Map Text Elpa)
readArchive path = mapException ArchiveError $ do
  let
    args = ["--eval", eval]
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"
  emacs args $ \out -> do
    result <- parseJsonFromStream out
    case result of
      Left parseError -> throwIO (ParseArchiveError parseError)
      Right pkgs -> pure pkgs

emacs :: [String] -> (InputByteStream -> IO a) -> IO a
emacs args go = do
  load <- getDataFileName "scripts/elpa2json.el"
  let
    args' = [ "-Q", "--batch", "--load", load ] ++ args
  runInteractiveProcess "emacs" args' Nothing Nothing go

parseJsonFromStream :: FromJSON a => InputByteStream -> IO (Either String a)
parseJsonFromStream stream = parseEither parseJSON <$> S.parseFromStream json' stream

-- * hashPackage

data DistNotImplemented = DistNotImplemented Text
  deriving (Show, Typeable)

instance Exception DistNotImplemented

hashPackage :: String -> HashMap Emacs.Name Name -> (Text, Elpa)
            -> IO (Maybe Package)
hashPackage server namesMap (name, pkg) =
  catchPretty $ do
    let
      -- ver can not be completely correct but works well for now
      -- see doc string of version-regexp-alist and version-to-list
      ver = (Elpa.ver pkg) &
        ((map (T.pack . show))
         >>> (T.intercalate ".")
         >>> (T.replace ".-4." "snapshot")
         >>> (T.replace ".-3." "alpha")
         >>> (T.replace ".-2." "beta")
         >>> (T.replace ".-1." "pre"))
      basename
        | null (Elpa.ver pkg) = T.unpack name
        | otherwise = T.unpack (name <> "-" <> ver)

    ext <- case Elpa.dist pkg of
            "single" -> pure "el"
            "tar" -> pure "tar"
            other -> throwIO (DistNotImplemented other)
    let
      url = server </> basename <.> ext
      fetch = Nix.URL { Nix.url = T.pack url
                      , Nix.sha256 = Nothing
                      , Nix.name = Nothing
                      }

    (_, fetcher) <- Nix.prefetch name fetch

    nixName <- Nix.getName namesMap (Emacs.Name name)
    nixDeps <- mapM (Nix.getName namesMap . Emacs.Name)
              $ filter (\dep -> dep /= "emacs") -- filter dummy dep emacs to reduce closure size
              $ (maybe [] M.keys (Elpa.deps pkg))

    pure Nix.Package
      { Nix.pname = nixName
      , Nix.ename = name
      , Nix.version = ver
      , Nix.fetch = fetcher
      , Nix.deps = nixDeps
      }
