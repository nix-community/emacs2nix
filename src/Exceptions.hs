{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2018 Thomas Tuegel

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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Exceptions
    ( module Control.Exception
    , showExceptions, showExceptions_, mapExceptionIO
    , NoRevision (..)
    , Died (..)
    , ProcessFailed (..)
    , ProcessingFailed (..)
    , ParseFilesError (..)
    , ManyExceptions (..), manyExceptions
    , PrettyException (..), catchPretty
    , mkException
    ) where

import Control.Exception
import Data.Text ( Text )
import qualified Data.Text as Text
import System.IO.Streams as Stream
import Text.PrettyPrint.ANSI.Leijen ( Pretty )
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Exceptions.TH


data NoRevision = NoRevision
mkException 'SomeException ''NoRevision


data Died = Died Int Text
mkException 'SomeException ''Died


data ProcessFailed = ProcessFailed String [String] SomeException
mkException 'SomeException ''ProcessFailed


data ProcessingFailed = ProcessingFailed Text Text SomeException
mkException 'SomeException ''ProcessingFailed


data ParseFilesError = ParseFilesError String
mkException 'SomeException ''ParseFilesError


showExceptions :: IO b -> IO (Maybe b)
showExceptions go = catch (Just <$> go) handler
  where
    handler (SomeException e) = do
      Stream.write (Just (Text.pack (show e))) =<< Stream.encodeUtf8 =<< Stream.unlines Stream.stdout
      pure Nothing


showExceptions_ :: IO b -> IO ()
showExceptions_ go = showExceptions go >> pure ()


mapExceptionIO :: (Exception e, Exception f) => (e -> f) -> IO a -> IO a
mapExceptionIO f go = catch go handler where
  handler e = throwIO (f e)


data PrettyException = forall e. (Exception e, Pretty e) => PrettyException e
mkException 'SomeException ''PrettyException

instance Pretty PrettyException where
  pretty (PrettyException e) = Pretty.pretty e


catchPretty :: IO a -> IO (Maybe a)
catchPretty action = catch (Just <$> action) handler
  where
    handler (PrettyException e) =
      do
        Pretty.putDoc (Pretty.pretty e)
        pure Nothing


data ManyExceptions = forall e. (Exception e, Pretty e) => ManyExceptions [e]
mkException 'PrettyException ''ManyExceptions

instance Pretty ManyExceptions where
  pretty (ManyExceptions es) =
    (Pretty.align . Pretty.vsep) (Pretty.pretty <$> es)

manyExceptions :: (Exception e, Pretty e) => [e] -> ManyExceptions
manyExceptions = ManyExceptions
