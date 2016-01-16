{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Index (updateIndex) where

import Control.Monad ( filterM )
import Data.List ( isPrefixOf, sort )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import System.Directory
       ( createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents )
import System.FilePath ((</>))
import qualified System.IO.Streams as S

import Distribution.Nix.Pretty hiding ((</>))

updateIndex :: FilePath  -- ^ output directory
            -> IO ()
updateIndex output = do
  createDirectoryIfMissing True output

  let
    packageNamesOnly path
      | "." `isPrefixOf` path = pure False -- skip special files
      | otherwise = doesDirectoryExist (output </> path)
                    -- each packages is a directory
  contents <- getDirectoryContents output >>= filterM packageNamesOnly

  let
    pnames = map T.pack contents
    writeIndex out = do
      let lbs = (T.encodeUtf8 . displayT . renderPretty 1 80)
                (pretty (packageIndex pnames))
      encoded <- S.fromLazyByteString lbs
      S.connect encoded out
    file = output </> "default.nix"
  S.withFileAsOutput file writeIndex

packageIndex :: [Text] -> Doc
packageIndex pnames
  = vsep [ "# DO NOT EDIT: generated automatically"
         , params [ "callPackage" ] ((attrs . map attr . sort) pnames)
         ]
  where
    attr _pname = ( (dquotes . text) _pname
                  , callPackage (text (T.append "./" _pname))
                  )
