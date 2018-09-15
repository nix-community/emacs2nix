{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2016 Thomas Tuegel

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Nix.Index ( readIndex, writeIndex ) where

import Control.Exception
import Data.Fix
import Data.List ( isPrefixOf )
import Data.List.NonEmpty ( NonEmpty (..) )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe ( isJust )
import Data.Text ( Text )
import qualified Data.Text as Text
import Nix.Parser ( NAssoc (..), OperatorInfo (..), Result (..), getBinaryOperator, getUnaryOperator, parseNixFile )
import Nix.Pretty hiding ( exprFNixDoc, prettyNix, prettyParams, prettyParamSet )
import Nix.Expr
import Prelude hiding ( (<$>) )
import System.Directory ( doesFileExist )
import System.IO.Streams ( OutputStream )
import qualified System.IO.Streams as S
import Text.PrettyPrint.ANSI.Leijen hiding ( sep )

import qualified Distribution.Emacs.Name as Emacs
import Distribution.Nix.Exception
import qualified Distribution.Nix.Name as Nix

readIndex :: FilePath  -- ^ output file
          -> IO (Map Nix.Name NExpr)
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
      | otherwise = pure Map.empty

writeIndex :: FilePath  -- ^ output file
           -> Map Nix.Name NExpr
           -> IO ()
writeIndex output packages = do
  S.withFileAsOutput output (write (packageIndex packages))
  where
    write index out = do
      let rendered = renderSmart 1.0 80 (prettyNix index)
      displayStream rendered =<< S.encodeUtf8 out

getFunctionBody :: NExpr -> Maybe NExpr
getFunctionBody (Fix (NAbs _ body)) = Just body
getFunctionBody _ = Nothing

getPackages :: NExpr -> Maybe (Map Nix.Name NExpr)
getPackages (Fix (NSet bindings)) =
  let
    getPackage (NamedVar (StaticKey name :| []) expr _) =
      Just (Nix.Name { Nix.fromName = name, Nix.ename = Emacs.Name name }, expr)
    getPackage _ = Nothing
  in
    fmap Map.fromList (traverse getPackage bindings)
getPackages _ = Nothing

packageIndex :: Map Nix.Name NExpr -> NExpr
packageIndex (Map.toList -> packages) = mkFunction args body where
  args = mkParamset [("callPackage", Nothing)] False
  body = (mkNonRecSet . map bindPackage) packages
  bindPackage (name, expr) = bindTo (Nix.fromName name) expr

displayStream :: SimpleDoc -> OutputStream Text -> IO ()
displayStream sdoc out = display sdoc where
  display SFail = throwIO PrettyFailed
  display SEmpty = return ()
  display (SChar c sdoc') = S.write (Just (Text.singleton c)) out >> display sdoc'
  display (SText _ t sdoc') = S.write (Just (Text.pack t)) out >> display sdoc'
  display (SLine i sdoc') = S.write (Just (Text.cons '\n' (indentation i))) out >> display sdoc'
  display (SSGR _ sdoc') = display sdoc'
  indentation i = Text.replicate i (Text.singleton ' ')

prettyParams :: Params NixDoc -> Doc
prettyParams (Param n) = text $ Text.unpack n
prettyParams (ParamSet s v mname) = prettyParamSet s v <> case mname of
  Nothing -> empty
  Just name | Text.null name -> empty
            | otherwise -> text "@" <> text (Text.unpack name)

prettyParamSet :: ParamSet NixDoc -> Bool -> Doc
prettyParamSet args var =
    encloseSep (lbrace <> space) (align (space <> rbrace)) sep (prettyArgs ++ prettyVariadic)
  where
    prettyArgs = (Map.elems . Map.mapWithKey prettySetArg . Map.fromList) args
    prettySetArg n maybeDef = case maybeDef of
      Nothing -> text (Text.unpack n)
      Just v -> text (Text.unpack n) <+> text "?" <+> withoutParens v
    prettyVariadic = [text "..." | var]
    sep = align (comma <> space)

exprFNixDoc :: NExprF NixDoc -> NixDoc
exprFNixDoc = \case
    NConstant atom -> prettyAtom atom
    NStr str -> simpleExpr $ prettyString str
    NList [] -> simpleExpr $ lbracket <> rbracket
    NList xs -> simpleExpr $ group $
        nest 2 (vsep $ lbracket : map (wrapParens appOpNonAssoc) xs) <$> rbracket
    NSet [] -> simpleExpr $ lbrace <> rbrace
    NSet xs -> simpleExpr $ group $
        nest 2 (vsep $ lbrace : map prettyBind xs) <$> rbrace
    NRecSet [] -> simpleExpr $ recPrefix <> lbrace <> rbrace
    NRecSet xs -> simpleExpr $ group $
        nest 2 (vsep $ recPrefix <> lbrace : map prettyBind xs) <$> rbrace
    NAbs args body -> leastPrecedence $
        nest 2 ((prettyParams args <> colon) <$> withoutParens body)
    NBinary NApp fun arg ->
        mkNixDoc (wrapParens appOp fun <+> wrapParens appOpNonAssoc arg) appOp
    NBinary op r1 r2 -> flip mkNixDoc opInfo $ hsep
        [ wrapParens (f NAssocLeft) r1
        , text $ Text.unpack $ operatorName opInfo
        , wrapParens (f NAssocRight) r2
        ]
      where
        opInfo = getBinaryOperator op
        f x | associativity opInfo /= x = opInfo { associativity = NAssocNone }
            | otherwise = opInfo
    NUnary op r1 ->
        mkNixDoc (text (Text.unpack (operatorName opInfo)) <> wrapParens opInfo r1) opInfo
      where opInfo = getUnaryOperator op
    NSelect r attr o ->
      (if isJust o then leastPrecedence else flip mkNixDoc selectOp) $
          wrapPath selectOp r <> dot <> prettySelector attr <> ordoc
      where ordoc = maybe empty (((space <> text "or") <+>) . wrapParens selectOp) o
    NHasAttr r attr ->
        mkNixDoc (wrapParens hasAttrOp r <+> text "?" <+> prettySelector attr) hasAttrOp
    NEnvPath p -> simpleExpr $ text ("<" ++ p ++ ">")
    NLiteralPath p -> pathExpr $ text $ case p of
        "./" -> "./."
        "../" -> "../."
        ".." -> "../."
        txt | "/" `isPrefixOf` txt -> txt
            | "~/" `isPrefixOf` txt -> txt
            | "./" `isPrefixOf` txt -> txt
            | "../" `isPrefixOf` txt -> txt
            | otherwise -> "./" ++ txt
    NSym name -> simpleExpr $ text (Text.unpack name)
    NLet binds body -> leastPrecedence $ group $ text "let" <$> indent 2 (
        vsep (map prettyBind binds)) <$> text "in" <+> withoutParens body
    NIf cond trueBody falseBody -> leastPrecedence $
        group $ nest 2 $ (text "if" <+> withoutParens cond) <$>
          (  align (text "then" <+> withoutParens trueBody)
         <$> align (text "else" <+> withoutParens falseBody)
          )
    NWith scope body -> leastPrecedence $
        text "with"  <+> withoutParens scope <> semi <$> align (withoutParens body)
    NAssert cond body -> leastPrecedence $
        text "assert" <+> withoutParens cond <> semi <$> align (withoutParens body)
  where
    recPrefix = text "rec" <> space

prettyNix :: NExpr -> Doc
prettyNix = withoutParens . cata exprFNixDoc
