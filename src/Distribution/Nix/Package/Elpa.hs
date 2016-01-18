{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Elpa ( Package(..), expression ) where

import Data.Fix
import Data.Text ( Text )
import qualified Data.Text as T
import Nix.Types

import Distribution.Nix.Builtin
import Distribution.Nix.Fetch ( Fetch, fetchExpr, importFetcher )
import Distribution.Nix.Name

data Package
  = Package
    { pname :: !Name
    , ename :: !Text
    , version :: !Text
    , fetch :: !Fetch
    , deps :: ![Name]
    }

expression :: Package -> NExpr
expression (Package {..}) = (mkSym "callPackage") `mkApp` drv `mkApp` emptySet where
  drv = mkFunction args body
  emptySet = mkNonRecSet []
  requires = map fromName deps
  args = (mkFixedParamSet . map optionalBuiltins)
         ("lib" : "elpaBuild" : importFetcher fetch : requires)
  body = (mkApp (mkSym "elpaBuild") . mkNonRecSet)
         [ "pname" `bindTo` mkStr DoubleQuoted (fromName pname)
         , "version" `bindTo` mkStr DoubleQuoted version
         , "src" `bindTo` fetchExpr fetch
         , "packageRequires" `bindTo` mkList (map mkSym requires)
         , "meta" `bindTo` meta
         ]
    where
      meta = mkNonRecSet
             [ "homepage" `bindTo` mkStr DoubleQuoted homepage
             , "license" `bindTo` license
             ]
        where
          homepage = T.concat
                     [ "http://elpa.gnu.org/packages/"
                     , ename
                     , ".html"
                     ]
          license = Fix (NSelect (mkSym "lib") [StaticKey "licenses", StaticKey "free"] Nothing)
