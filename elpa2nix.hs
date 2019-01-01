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

module Main (main) where

import Control.Concurrent ( setNumCapabilities )
import Control.Concurrent.Async ( Concurrently(..) )
import Control.Exception
import Control.Monad ( join, when )
import Data.Aeson ( FromJSON(..), json' )
import Data.Aeson.Types ( parseEither )
import Data.ByteString ( ByteString )
import Data.HashMap.Strict ( HashMap )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
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
import Distribution.Nix.Name ( Name )
import Distribution.Nix.Package.Elpa ( Package )
import Exceptions
import Process

import qualified Distribution.Elpa as Elpa
import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Index as Nix
import qualified Distribution.Nix.Name as Nix
import qualified Distribution.Nix.Package.Elpa as Nix

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

elpa2nix :: Int -> FilePath -> String -> FilePath -> IO ()
elpa2nix threads output server namesMapFile =
  catchPretty_ $ do
    when (threads > 0) (setNumCapabilities threads)
    namesMap <- Nix.readNames namesMapFile
    archives <- getPackages server
    let update = traverse (updatePackage server namesMap) (Map.toList archives)
    packages <- runConcurrently (Map.fromList . catMaybes <$> update)
    Nix.writeIndex output packages

updatePackage
    :: String
    -> HashMap Emacs.Name Name
    -> (Text, Elpa)
    -> Concurrently (Maybe (Nix.Name, NExpr))
updatePackage server namesMap elpa = Concurrently $ do
  hashed <- hashPackage server namesMap elpa
  pure (toExpression <$> hashed)
  where
    toExpression pkg = (Nix.pname pkg, Nix.expression pkg)

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
      ver = T.intercalate "." (map (T.pack . show) (Elpa.ver pkg))
      basename
        | null (Elpa.ver pkg) = T.unpack name
        | otherwise = T.unpack (name <> "-" <> ver)

    ext <- case Elpa.dist pkg of
            "single" -> pure "el"
            "tar" -> pure "tar"
            other -> throwIO (DistNotImplemented other)
    let
      url = server </> basename <.> ext
      fetch =
          Nix.fetchUrl Nix.Url
              { url = T.pack url
              , sha256 = Nothing
              , name = Nothing
              }

    (_, fetcher) <- Nix.prefetch name fetch

    nixName <- Nix.getName namesMap (Emacs.Name name)
    nixDeps <- mapM (Nix.getName namesMap . Emacs.Name)
              (maybe [] Map.keys (Elpa.deps pkg))

    pure Nix.Package
      { Nix.pname = nixName
      , Nix.ename = name
      , Nix.version = ver
      , Nix.fetch = fetcher
      , Nix.deps = nixDeps
      }
