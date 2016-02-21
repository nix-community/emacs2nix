{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Nix.Index ( readIndex, writeIndex ) where

import Control.Exception
import Data.Fix
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import Data.Text ( Text )
import qualified Data.Text as T
import Nix.Parser ( Result(..), parseNixFile )
import Nix.Pretty ( prettyNix )
import Nix.Types
import System.Directory ( doesFileExist )
import System.IO.Streams ( OutputStream )
import qualified System.IO.Streams as S
import Text.PrettyPrint.ANSI.Leijen hiding ( (<$>) )

import Distribution.Nix.Exception
import Distribution.Nix.Name ( Name, fromName, fromText )

readIndex :: FilePath  -- ^ output file
          -> IO (Map Name NExpr)
readIndex output = parseOutputOrDefault =<< doesFileExist output
  where
    die = throwIO (InvalidIndex output)

    parseOutputOrDefault exists
      | exists = do
          result <- parseNixFile output
          case result of
            Failure _ -> die
            Success parsed ->
              maybe die pure (getFunctionBody parsed >>= getPackages)
      | otherwise = pure M.empty

writeIndex :: FilePath  -- ^ output file
           -> Map Name NExpr
           -> IO ()
writeIndex output packages = do
  S.withFileAsOutput output (write (packageIndex packages))
  where
    write index out = do
      let rendered = renderPretty 1 80 (prettyNix index)
      displayStream rendered =<< S.encodeUtf8 out

getFunctionBody :: NExpr -> Maybe NExpr
getFunctionBody (Fix (NAbs _ body)) = Just body
getFunctionBody _ = Nothing

getPackages :: NExpr -> Maybe (Map Name NExpr)
getPackages (Fix (NSet NonRec bindings)) =
  let
    getPackage (NamedVar [StaticKey name] expr) = Just (fromText name, expr)
    getPackage _ = Nothing
  in
    M.fromList <$> traverse getPackage bindings
getPackages _ = Nothing

packageIndex :: Map Name NExpr -> NExpr
packageIndex (M.toList -> packages) = mkFunction args body where
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
