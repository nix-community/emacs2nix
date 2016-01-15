{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Elpa ( Package(..) ) where

import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch, importFetcher )
import Distribution.Nix.Name
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
        (attrs [ ("pname", (dquotes . text)
                           (T.append "emacs-" (fromName pname)))
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
          homepage = (dquotes . text)
                     (T.concat
                      [ "http://elpa.gnu.org/packages/"
                      , ename
                      , ".html"
                      ])
          license = "lib.licenses.free";
        in
          attrs [("homepage", homepage), ("license", license)]
