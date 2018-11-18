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

{-# LANGUAGE TemplateHaskell #-}

module Exceptions
    ( module Control.Monad.Catch
    , NoRevision (..)
    , Died (..)
    , ProcessFailed (..)
    , ProcessingFailed (..)
    , ParseFilesError (..)
    , ManyExceptions (..), manyExceptions
    , PrettyException (..), catchPretty, catchPretty_
    , mkException
    , Context (..), inContext
    , DeferredErrors (..)
    ) where

import Control.Monad.Catch
import Data.Semigroup
import Data.Text ( Text )
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen ( Doc, Pretty, (<+>) )
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Exceptions.TH


data NoRevision = NoRevision
mkException 'SomeException ''NoRevision


data ProcessFailed = ProcessFailed String [String] SomeException
mkException 'SomeException ''ProcessFailed


data ProcessingFailed = ProcessingFailed Text Text SomeException
mkException 'SomeException ''ProcessingFailed


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


catchPretty_ :: IO () -> IO ()
catchPretty_ action = catch action handler
  where
    handler (PrettyException e) = Pretty.putDoc (Pretty.pretty e)


data ManyExceptions = forall e. (Exception e, Pretty e) => ManyExceptions [e]
mkException 'PrettyException ''ManyExceptions

instance Pretty ManyExceptions where
  pretty (ManyExceptions es) =
    (Pretty.align . Pretty.vsep) (Pretty.pretty <$> es)


manyExceptions :: (Exception e, Pretty e) => [e] -> ManyExceptions
manyExceptions = ManyExceptions


data Context =
  forall e. (Exception e, Pretty e) =>
  Context { context :: Doc, exception :: e }
mkException 'PrettyException ''Context

instance Pretty Context where
  pretty Context {..} =
    "in " <> context <> ": " <> Pretty.pretty exception


mapExceptionM :: (Exception e1, Exception e2, MonadCatch m, MonadThrow m)
              => (e1 -> e2) -> m a -> m a
mapExceptionM f = handle (\e1 -> throwM (f e1))


inContext :: (MonadCatch m, MonadThrow m) => Doc -> m a -> m a
inContext context =
  mapExceptionM (\(PrettyException exception) -> Context {..})


data DeferredErrors = DeferredErrors
mkException 'PrettyException ''DeferredErrors


instance Pretty DeferredErrors where
  pretty DeferredErrors = "deferred error(s) above"


data Died = Died Int Text
mkException 'PrettyException ''Died


instance Pretty Died where
  pretty (Died exit err) =
    Pretty.vsep
    [ "died with exit code" <+> Pretty.pretty exit <> ":"
    , Pretty.string (Text.unpack err)
    ]


data ParseFilesError = ParseFilesError String
mkException 'PrettyException ''ParseFilesError


instance Pretty ParseFilesError where
  pretty (ParseFilesError err) =
    Pretty.vsep
    [ "parse error:"
    , Pretty.string err
    ]
