{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Melpa ( Package(..), Recipe(..) ) where

import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch, importFetcher )
import Distribution.Nix.Name
import Distribution.Nix.Pretty

data Package
  = Package
    { pname :: !Name
    , version :: !Text
    , fetch :: !Fetch
    , deps :: ![Name]
    , recipe :: !Recipe
    }
  deriving Generic

instance Pretty Package where
  pretty (Package {..})
    = vsep
      [ "# DO NOT EDIT: generated automatically"
      , (params imports . melpaBuild)
        (attrs [ ("pname", (dquotes . text)
                           (T.append "emacs-" (fromName pname)))
               , ("version", (dquotes . text) version)
               , ("src", pretty fetch)
               , ("recipeFile", pretty recipe)
               , ("packageRequires", list packageRequires)
               , ("meta", meta)
               ])
      ]
    where
      packageRequires = map pretty deps
      imports = "lib" : "melpaBuild" : importFetcher fetch : packageRequires

      meta =
        let
          homepage = (dquotes . cat) [ "http://melpa.org/#/", text (ename recipe) ]
          license = "lib.licenses.free";
        in
          attrs [("homepage", homepage), ("license", license)]

data Recipe
  = Recipe { ename :: !Text
           , commit :: !Text
           , sha256 :: !Text
           }
  deriving Generic

instance Pretty Recipe where
  pretty (Recipe {..})
    = (fetchurl . attrs)
      [ ("url", (dquotes . cat)
                [ "https://raw.githubusercontent.com/milkypostman/melpa/"
                , text commit
                , "/recipes/"
                , text ename
                ])
      , ("sha256", (dquotes . text) sha256)
      ]
