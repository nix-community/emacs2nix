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

{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nix.Package.Melpa
    ( Package(..)
    , Recipe(..)
    , expression
    , writePackageExpression
    , readPackageExpression
    ) where

import qualified Control.Exception as Exception
import Data.Fix
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Version ( Version, showVersion )
import Nix.Expr
import qualified Nix.Parser
import qualified Nix.Pretty
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Streams as Streams
import qualified System.IO.Temp as Temp
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import qualified Distribution.Emacs.Name as Emacs
import Distribution.Nix.Builtin
import Distribution.Nix.Fetch ( Fetch, fetchExpr, importFetcher )
import qualified Distribution.Nix.Name as Nix
import Exceptions
import qualified System.IO.Streams.Pretty as Pretty

data Package =
  Package
    { pname :: !Nix.Name
    , version :: !Version
    , fetch :: !Fetch
    , deps :: ![Nix.Name]
    , recipe :: !Recipe
    }

data Recipe =
  Recipe
    { ename :: !Emacs.Name
    , commit :: !Text
    , sha256 :: !Text
    }

expression :: Package -> NExpr
expression (Package {..}) =
    mkFunction args body
  where
    requires = map Nix.fromName deps
    args = (flip mkParamset False . map optionalBuiltins)
          ("lib" : "melpaBuild" : "fetchurl" : importFetcher fetch : requires)
    body = ((@@) (mkSym "melpaBuild") . mkNonRecSet)
          [ "pname" `bindTo` mkStr (Nix.fromName pname)
          , "ename" `bindTo` mkStr tname
          , "version" `bindTo` mkStr (Text.pack $ showVersion version)
          , "src" `bindTo` fetchExpr fetch
          , "recipe" `bindTo` fetchRecipe
          , "packageRequires" `bindTo` mkList (map mkSym requires)
          , "meta" `bindTo` meta
          ]
      where
        Recipe { ename, commit } = recipe
        tname = Emacs.fromName ename
        meta = mkNonRecSet
              [ "homepage" `bindTo` mkStr homepage
              , "license" `bindTo` license
              ]
          where
            homepage = Text.append "https://melpa.org/#/" tname
            license =
              Fix (NSelect (mkSym "lib")
                  [StaticKey "licenses", StaticKey "free"] Nothing)
        fetchRecipe = ((@@) (mkSym "fetchurl") . mkNonRecSet)
                      [ "url" `bindTo` mkStr
                        (Text.concat
                        [ "https://raw.githubusercontent.com/melpa/melpa/"
                        , commit
                        , "/recipes/"
                        , tname
                        ])
                      , "sha256" `bindTo` mkStr (sha256 recipe)
                      , "name" `bindTo` mkStr "recipe"
                      ]


writePackageExpression
  :: FilePath
  -> NExpr
  -> IO ()
writePackageExpression output expr =
  do
    let (directory, filename) = FilePath.splitFileName output
    tmp <- Temp.emptyTempFile directory filename
    Streams.withFileAsOutput tmp
      (\out ->
        do
          let
            doc = Nix.Pretty.prettyNix expr
            rendered = Pretty.renderSmart 1.0 80 doc
          Pretty.displayStream rendered =<< Streams.encodeUtf8 out
      )
    Directory.renameFile tmp output
  where


data NixParseFailure = NixParseFailure Pretty.Doc
mkException 'PrettyException ''NixParseFailure


instance Pretty.Pretty NixParseFailure where
  pretty (NixParseFailure failed) =
    "Failed to parse expression:" Pretty.<+> failed


readPackageExpression :: FilePath -> IO NExpr
readPackageExpression input =
  do
    result <- Nix.Parser.parseNixFile input
    case result of
      Nix.Parser.Failure failed ->
        Exception.throwIO (NixParseFailure failed)
      Nix.Parser.Success parsed ->
        return parsed
