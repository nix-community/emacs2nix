{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Elpa
       ( Package(..), packageSet ) where

import Data.List ( sort )
import Data.Text ( Text )
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch, importFetcher )
import Distribution.Nix.Name ( Name )
import Distribution.Nix.Pretty

data Package
  = Package
    { pname :: !Name
    , ename :: !Text
    , version :: !Text
    , fetch :: !Fetch
    , deps :: ![Name]
    }
  deriving Generic

instance Pretty Package where
  pretty (Package {..})
    = vsep
      [ "# DO NOT EDIT: generated automatically"
      , (params imports . elpaBuild)
        (attrs [ ("pname", (dquotes . pretty) pname)
               , ("version", (dquotes . text) version)
               , ("src", pretty fetch)
               , ("packageRequires", list packageRequires)
               , ("meta", meta)
               ])
      ]
    where
      packageRequires = map pretty deps
      imports = "lib" : "elpaBuild" : importFetcher fetch : packageRequires

      meta =
        let
          homepage = (dquotes . cat)
                     [ "http://elpa.gnu.org/packages/", text ename, ".html" ]
          license = "lib.licenses.free";
        in
          attrs [("homepage", homepage), ("license", license)]

packageSet :: [Text] -> Doc
packageSet pnames
  = vsep [ "# DO NOT EDIT: generated automatically"
         , params [ "callPackage" ] ((attrs . map attr . sort) pnames)
         ]
  where
    attr _pname = ( (dquotes . text) _pname
                  , callPackage (cat ["./", text _pname])
                  )
