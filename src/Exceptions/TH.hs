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


{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Exceptions.TH where

import Control.Exception
import Data.Typeable
import Language.Haskell.TH
import qualified Language.Haskell.TH.Lib as TH


-- | Declare a heirarchical instance of 'Exception' for the type @child@ wrapped
-- in the @parent@ data constructor. The parent name is a /value/ name (quoted
-- with @'@ in Template Haskell) for a data constructor in case the parent type
-- has multiple constructors. The child name is a /type/ name (quoted with @''@
-- in Template Haskell).
--
-- The required instances of 'Show' and 'Typeable' are automatically derived.
-- The exception type need only be declared.
mkException :: Name  -- ^ parent exception constructor name
            -> Name  -- ^ child exception type name
            -> Q [Dec]
mkException parent child =
  do
    e <- newName "e"
    [d|
      deriving instance Show $(TH.conT child)
      deriving instance Typeable $(TH.conT child)

      instance Exception $(TH.conT child) where
        toException $(TH.varP e) = toException ($(TH.conE parent) $(TH.varE e))
        fromException f =
          do
            $(TH.conP parent [TH.varP e]) <- fromException f
            cast $(TH.varE e)
      |]
