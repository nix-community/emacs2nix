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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Melpa ( Package(..), Recipe(..), expression ) where

import Data.Fix
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Version ( Version, showVersion )
import Nix.Expr

import Distribution.Nix.Builtin
import Distribution.Nix.Fetch ( Fetch, fetchExpr, importFetcher )
import Distribution.Nix.Name

data Package
  = Package
    { pname :: !Name
    , version :: !Version
    , fetch :: !Fetch
    , deps :: ![Name]
    , recipe :: !Recipe
    }

data Recipe
  = Recipe { ename :: !Text
           , commit :: !Text
           , sha256 :: !Text
           }

expression :: Package -> NExpr
expression (Package {..}) = (mkSym "callPackage") @@ drv @@ emptySet where
  drv = mkFunction args body
  emptySet = mkNonRecSet []
  requires = map fromName deps
  args = (flip mkParamset False . map optionalBuiltins)
         ("lib" : "melpaBuild" : "fetchurl" : importFetcher fetch : requires)
  body = ((@@) (mkSym "melpaBuild") . mkNonRecSet)
         [ "pname" `bindTo` mkStr (fromName pname)
         , "version" `bindTo` mkStr (T.pack $ showVersion version)
         , "src" `bindTo` fetchExpr fetch
         , "recipe" `bindTo` fetchRecipe
         , "packageRequires" `bindTo` mkList (map mkSym requires)
         , "meta" `bindTo` meta
         ]
    where
      Recipe { ename, commit } = recipe
      meta = mkNonRecSet
             [ "homepage" `bindTo` mkStr homepage
             , "license" `bindTo` license
             ]
        where
          homepage = T.append "https://melpa.org/#/" ename
          license =
            Fix (NSelect (mkSym "lib")
                 [StaticKey "licenses", StaticKey "free"] Nothing)
      fetchRecipe = ((@@) (mkSym "fetchurl") . mkNonRecSet)
                    [ "url" `bindTo` mkStr
                      (T.concat
                       [ "https://raw.githubusercontent.com/milkypostman/melpa/"
                       , commit
                       , "/recipes/"
                       , ename
                       ])
                    , "sha256" `bindTo` mkStr (sha256 recipe)
                    , "name" `bindTo` mkStr "recipe"
                    ]
