{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Index (updateIndex) where

import Control.Exception
import Data.Fix
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import Nix.Parser ( Result(..), parseNixFile )
import Nix.Pretty ( prettyNix )
import Nix.Types
import System.Directory ( doesFileExist )
import System.IO.Streams ( OutputStream )
import qualified System.IO.Streams as S
import Text.PrettyPrint.ANSI.Leijen

import Distribution.Nix.Exception
import Distribution.Nix.Name ( Name, fromName, fromText )

updateIndex :: FilePath  -- ^ output file
            -> [(Name, NExpr)]
            -> IO ()
updateIndex output updated = do
  existing <- parseOutputOrDefault =<< doesFileExist output
  let
    packages = M.toList (M.union (M.fromList updated) (M.fromList existing))
    index = packageIndex packages
  S.withFileAsOutput output (writeIndex index)
  where
    die = throwIO (InvalidIndex output)

    parseOutputOrDefault exists
      | exists = do
          result <- parseNixFile output
          case result of
            Failure _ -> die
            Success parsed -> maybe die pure (getFunctionBody parsed >>= getPackages)
      | otherwise = pure []

    writeIndex index out = do
      let rendered = renderPretty 1 80 (prettyNix index)
      displayStream rendered =<< S.encodeUtf8 out

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
  bindPackage (name, expr) = bindTo (fromName name) expr

displayStream :: SimpleDoc -> OutputStream Text -> IO ()
displayStream sdoc out = display sdoc where
  display SFail = throwIO PrettyFailed
  display SEmpty = return ()
  display (SChar c sdoc') = S.write (Just (T.singleton c)) out >> display sdoc'
  display (SText _ t sdoc') = S.write (Just (T.pack t)) out >> display sdoc'
  display (SLine i sdoc') = S.write (Just (T.cons '\n' (indentation i))) out >> display sdoc'
  display (SSGR _ sdoc') = display sdoc'
  indentation i = T.replicate i (T.singleton ' ')
