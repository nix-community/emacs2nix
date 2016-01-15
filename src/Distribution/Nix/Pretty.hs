{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Pretty
       ( module Export
       , text, parens'
       , list, attrs, params
       , callPackage, elpaBuild, melpaBuild
       , fetchurl, fetchgit, fetchbzr, fetchcvs, fetchhg, fetchsvn
       , fetchFromGitHub, fetchFromGitLab
       ) where

import qualified Text.PrettyPrint.Leijen.Text as Export hiding ( (<>), (<$>), list, text )

import Data.Text ( Text )
import Data.Text.Lazy ( fromStrict )
import Text.PrettyPrint.Leijen.Text hiding ( list, text )
import qualified Text.PrettyPrint.Leijen.Text as PPrint

text :: Text -> Doc
text = PPrint.text . fromStrict

parens' :: Doc -> Doc
parens' doc = vsep [ "(", indent 2 doc, ")" ]

list :: [Doc] -> Doc
list = enclose "[" "]" . align . fillSep

attrs :: [(Doc, Doc)] -> Doc
attrs pairs = vsep [ "{"
                   , (indent 2 . vsep) (map attr pairs)
                   , "}"
                   ]
  where
    attr (key, val) = key <+> equals <+> val <> semi

params :: [Doc] -> Doc -> Doc
params names body
  = vsep [ align (encloseSep "{" "}:" "," names)
         , body
         ]

callPackage :: Doc -> Doc
callPackage body = "callPackage" <+> body <+> braces empty

elpaBuild :: Doc -> Doc
elpaBuild = ("elpaBuild" <+>)

melpaBuild :: Doc -> Doc
melpaBuild = ("melpaBuild" <+>)

fetchurl :: Doc -> Doc
fetchurl = ("fetchurl" <+>)

fetchgit :: Doc -> Doc
fetchgit = ("fetchgit" <+>)

fetchbzr :: Doc -> Doc
fetchbzr = ("fetchbzr" <+>)

fetchcvs :: Doc -> Doc
fetchcvs = ("fetchcvs" <+>)

fetchhg :: Doc -> Doc
fetchhg = ("fetchhg" <+>)

fetchsvn :: Doc -> Doc
fetchsvn = ("fetchsvn" <+>)

fetchFromGitHub :: Doc -> Doc
fetchFromGitHub = ("fetchFromGitHub" <+>)

fetchFromGitLab :: Doc -> Doc
fetchFromGitLab = ("fetchFromGitLab" <+>)
