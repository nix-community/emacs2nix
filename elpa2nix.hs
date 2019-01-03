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
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Typeable ( Typeable )
import Nix.Expr ( NExpr )
import Options.Applicative
import System.FilePath ( (</>), (<.>) )
import System.IO ( hClose )
import qualified System.IO.Streams as S
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as S
import System.IO.Temp ( withSystemTempFile )

import qualified Paths_emacs2nix as Paths

import Distribution.Elpa ( Elpa )
import Distribution.Nix.Package.Elpa ( Package )
import Exceptions
import Process

import qualified Distribution.Elpa as Elpa
import qualified Distribution.Emacs.Name as Emacs
import qualified Distribution.Nix.Fetch as Nix
import qualified Distribution.Nix.Index as Nix
import qualified Distribution.Nix.Package.Elpa as Nix

main :: IO ()
main = join (execParser (info (helper <*> parser) desc))
  where
    desc = fullDesc <> progDesc "Generate Nix expressions from ELPA"

parser :: Parser (IO ())
parser =
  elpa2nix
  <$> (threads <|> pure 0)
  <*> server
  where
    threads = option auto
              (long "threads" <> short 't'
               <> metavar "N"
               <> help "use N threads; default is number of CPUs")
    server = strArgument
             (metavar "URL"
              <> help "get packages from server at URL")

elpa2nix :: Int -> String -> IO ()
elpa2nix threads server =
  catchPretty_ $ do
    when (threads > 0) (setNumCapabilities threads)
    archives <- getPackages server
    let update = updatePackage server
    packages <- runConcurrently (Map.traverseMaybeWithKey update archives)
    output <- Streams.encodeUtf8 Streams.stdout
    Nix.writeIndex output packages

updatePackage
    :: String
    -> Emacs.Name
    -> Elpa
    -> Concurrently (Maybe NExpr)
updatePackage server ename elpa =
    concurrently (Nix.expression <$> hashPackage server ename elpa)
  where
    concurrently = Concurrently . catchPretty

-- * Error types

newtype ArchiveError = ArchiveError SomeException
  deriving (Show, Typeable)

instance Exception ArchiveError

-- * getPackages

getPackages :: String -> IO (Map Emacs.Name Elpa)
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

readArchive :: FilePath -> IO (Map Emacs.Name Elpa)
readArchive path = mapException ArchiveError $ do
  let
    args = ["--eval", eval]
    eval = "(print-archive-contents-as-json " ++ show path ++ ")"
  emacs args $ \out -> do
    result <- parseJsonFromStream out
    case result of
      Left parseError -> throwIO (ParseArchiveError parseError)
      Right pkgs -> pure (Map.mapKeys Emacs.Name pkgs)

emacs :: [String] -> (InputByteStream -> IO a) -> IO a
emacs args go = do
  load <- Paths.getDataFileName "scripts/elpa2json.el"
  let
    args' = [ "-Q", "--batch", "--load", load ] ++ args
  runInteractiveProcess "emacs" args' Nothing Nothing go

parseJsonFromStream :: FromJSON a => InputByteStream -> IO (Either String a)
parseJsonFromStream stream = parseEither parseJSON <$> S.parseFromStream json' stream

-- * hashPackage

data DistNotImplemented = DistNotImplemented Text
  deriving (Show, Typeable)

instance Exception DistNotImplemented

hashPackage :: String -> Emacs.Name -> Elpa -> IO Package
hashPackage server ename elpa =
  do
    let
      Elpa.Elpa { ver } = elpa
      version = T.intercalate "." (T.pack . show <$> ver)
      basename
        | null ver = T.unpack tname
        | otherwise = T.unpack (tname <> "-" <> version)
        where
          tname = Emacs.fromName ename

    ext <- case Elpa.dist elpa of
            "single" -> pure "el"
            "tar" -> pure "tar"
            other -> throwIO (DistNotImplemented other)
    let
      url = server </> basename <.> ext
      prefetch =
          Nix.fetchUrl Nix.Url
              { url = T.pack url
              , sha256 = Nothing
              , name = Nothing
              }

    (_, fetch) <- Nix.prefetch prefetch

    let deps = Emacs.Name <$> maybe [] Map.keys (Elpa.deps elpa)

    pure Nix.Package { ename, version, fetch, deps }
