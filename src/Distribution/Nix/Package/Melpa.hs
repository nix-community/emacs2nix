{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nix.Package.Melpa ( Package(..), Recipe(..) ) where

import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics

import Distribution.Nix.Fetch ( Fetch(URL), importFetcher )
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
      fetchers =
        case fetch of
          URL {} -> [ importFetcher fetch ]
          _ -> [ "fetchurl", importFetcher fetch ]
      imports = "lib" : "melpaBuild" : (fetchers ++ packageRequires)

      meta =
        let
          homepage = (dquotes . text)
                     (T.append "http://melpa.org/#/" (ename recipe))
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
      [ ("url", (dquotes . text)
                (T.concat
                 [ "https://raw.githubusercontent.com/milkypostman/melpa/"
                 , commit
                 , "/recipes/"
                 , ename
                 ]))
      , ("sha256", (dquotes . text) sha256)
      ]
