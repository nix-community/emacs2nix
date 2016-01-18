{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Index (updateIndex) where

import Control.Exception
import Data.Fix
import qualified Data.Map as M
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Nix.Parser ( Result(..), parseNixFile )
import Nix.Types
import qualified System.IO.Streams as S

import Distribution.Nix.Exception
import Distribution.Nix.Name ( Name, fromName, fromText )

updateIndex :: FilePath  -- ^ output file
            -> [(Name, NExpr)]
            -> IO ()
updateIndex output updated = do
  result <- parseNixFile output
  existing <- case result of
    Failure _ -> die
    Success parsed -> maybe die pure (getFunctionBody parsed >>= getPackages)
  let
    packages = M.toList (M.union (M.fromList updated) (M.fromList existing))
    index = packageIndex packages
  S.withFileAsOutput output (writeIndex index)
  where
    die = throwIO (InvalidIndex output)

    writeIndex index out = do
      pure ()

getFunctionBody :: NExpr -> Maybe NExpr
getFunctionBody (Fix (NAbs _ body)) = Just body
getFunctionBody _ = Nothing

getPackages :: NExpr -> Maybe [(Name, NExpr)]
getPackages (Fix (NSet NonRec bindings)) =
  let
    getPackage (NamedVar [StaticKey name] expr) = Just (fromText name, expr)
    getPackage _ = Nothing
  in
    traverse getPackage bindings
getPackages _ = Nothing

packageIndex :: [(Name, NExpr)] -> NExpr
packageIndex packages = mkFunction args body where
  args = mkFixedParamSet [("callPackage", Nothing)]
  body = (mkNonRecSet . map bindPackage) packages
  bindPackage (name, expr) = bindTo (fromName name)
                             (mkApp
                              (mkApp (mkSym "callPackage") expr)
                              (mkNonRecSet []))
