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

module Distribution.Nix.Package.Melpa
    ( Package(..)
    , Recipe(..)
    , expression
    , express
    ) where

import qualified Control.Monad.Extra as Monad
import Data.Fix
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Version ( Version, showVersion )
import Nix.Expr
import qualified Nix.Pretty
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Streams as Streams
import qualified System.IO.Temp as Temp
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import qualified Distribution.Emacs.Name as Emacs
import Distribution.Nix.Builtin
import Distribution.Nix.Fetch ( Fetch, fetchExpr, importFetcher )
import Distribution.Nix.Name
import qualified System.IO.Streams.Pretty as Pretty

data Package =
  Package
    { pname :: !Name
    , version :: !Version
    , fetch :: !Fetch
    , deps :: ![Name]
    , recipe :: !Recipe
    }

data Recipe =
  Recipe
    { ename :: !Text
    , commit :: !Text
    , sha256 :: !Text
    }

expression :: Package -> NExpr
expression (Package {..}) =
    mkFunction args body
  where
    requires = map fromName deps
    args = (flip mkParamset False . map optionalBuiltins)
          ("lib" : "melpaBuild" : "fetchurl" : importFetcher fetch : requires)
    body = ((@@) (mkSym "melpaBuild") . mkNonRecSet)
          [ "pname" `bindTo` mkStr (fromName pname)
          , "ename" `bindTo` mkStr ename
          , "version" `bindTo` mkStr (Text.pack $ showVersion version)
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
            homepage = Text.append "https://melpa.org/#/" ename
            license =
              Fix (NSelect (mkSym "lib")
                  [StaticKey "licenses", StaticKey "free"] Nothing)
        fetchRecipe = ((@@) (mkSym "fetchurl") . mkNonRecSet)
                      [ "url" `bindTo` mkStr
                        (Text.concat
                        [ "https://raw.githubusercontent.com/melpa/melpa/"
                        , commit
                        , "/recipes/"
                        , ename
                        ])
                      , "sha256" `bindTo` mkStr (sha256 recipe)
                      , "name" `bindTo` mkStr "recipe"
                      ]


express
  :: FilePath
  -> Package
  -> IO ()
express outpath package =
  do
    let
      filename :: FilePath
      filename =
          concat
          ([ Text.unpack (Emacs.fromName ename)
          , "-"
          , showVersion version
          , ".nix"
          ] :: [String])
        where
          Package { pname, version } = package
          Name { ename } = pname
      output = outpath </> filename
    -- If the output path exists, assume it is up-to-date.
    Monad.unlessM (Directory.doesPathExist output)
      (do
        tmp <- Temp.emptyTempFile outpath filename
        Streams.withFileAsOutput tmp writePackage0
        Directory.renameFile tmp output
      )
  where
    writePackage0 out =
      do
        let
          expr = expression package
          rendered = Pretty.renderSmart 1.0 80 (Nix.Pretty.prettyNix expr)
        Pretty.displayStream rendered =<< Streams.encodeUtf8 out
