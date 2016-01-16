{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Index (updateIndex) where

import Data.List ( sortOn )
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import qualified System.IO.Streams as S

import Distribution.Nix.Name ( Name )
import Distribution.Nix.Pretty hiding ((</>))

updateIndex :: [(Name, Doc)]  -- ^ packages
            -> FilePath  -- ^ output path
            -> IO ()
updateIndex packages path = do
  let
    writeIndex out = do
      let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80)
                (packageIndex packages)
      encoded <- S.fromLazyByteString lbs
      S.connect encoded out
  S.withFileAsOutput path writeIndex

packageIndex :: [(Name, Doc)] -> Doc
packageIndex packages
  = vsep [ "# DO NOT EDIT: generated automatically"
         , params [ "callPackage" ] ((attrs . map attr . sortOn fst) packages)
         ]
  where
    attr (name, expr) = (pretty name, callPackage expr)
