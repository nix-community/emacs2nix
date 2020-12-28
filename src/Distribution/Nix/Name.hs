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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Nix.Name
  ( Name (..)
  , readNames
  , fromText, getName
  , InvalidName (..)
  , ReadNamesError (..)
  , ParseNixError (..)
  ) where

import qualified Data.Char as Char
import Data.Fix ( Fix (..) )
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time.Clock ( getCurrentTime )
import Nix.Exec ( evalExprLoc, runLazyM )
import Nix.Normal ( normalForm )
import Nix.Options ( defaultOptions )
import Nix.Parser
import Nix.Value
import Text.PrettyPrint.ANSI.Leijen ( Doc, Pretty (..) )
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import qualified Distribution.Emacs.Name as Emacs
import Exceptions


-- | A valid Nix package name.
data Name =
  Name
  {
    fromName :: Text,
    ename :: Emacs.Name
  }
  deriving (Eq, Ord, Show)

instance Hashable Name where
  hashWithSalt salt name = hashWithSalt salt (fromName name)


data InvalidName = forall e. (Exception e, Pretty e) => InvalidName e
mkException 'PrettyException ''InvalidName

instance Pretty InvalidName where
  pretty (InvalidName e) =
    "invalid name: " <> Pretty.pretty e


-- | An exception thrown if @invalidName@ is invalid because it begins with a
-- digit.
--
-- Parent: 'InvalidName'
data LeadingDigit = LeadingDigit { invalidName :: Text }
mkException 'InvalidName ''LeadingDigit

instance Pretty LeadingDigit where
  pretty LeadingDigit {..} =
    "'" <> Pretty.string (Text.unpack invalidName) <> "' begins with a digit"


-- | An exception thrown if @invalidName@ is invalid due to the illegal
-- character @illegalChar@.
--
-- Parent: 'InvalidName'
data IllegalChar = IllegalChar { invalidName :: Text, illegalChar :: Char }
mkException 'InvalidName ''IllegalChar

instance Pretty IllegalChar where
  pretty IllegalChar {..} =
    "'" <> Pretty.string (Text.unpack invalidName)
    <> "' contains illegal character '" <> Pretty.char illegalChar <> "'"

-- | Characters that may not appear in a Nix identifier.
illegalChars :: Set Char
illegalChars = Set.fromList ['@', '+']


-- | Decode a valid Nix package name from 'Text'; if the name is invalid, an
-- exception is thrown. Valid Nix names must not begin with a digit or contain
-- the characters in 'illegalChars'.
--
-- Throws: 'LeadingDigit', 'IllegalChar'
fromText :: MonadThrow m => Emacs.Name -> m Name
fromText name =
  case getFirst (Text.foldl' firstIllegal mempty txt) of
    Nothing
      | leadingDigit -> throwM (LeadingDigit { invalidName = txt })
      | otherwise -> pure Name { fromName = txt, ename = name }
    Just illegal ->
      throwM (IllegalChar { invalidName = txt, illegalChar = illegal })
  where
    txt = Emacs.fromName name
    leadingDigit = Char.isDigit (Text.head txt)

    -- | Find the first illegal character in the name.
    firstIllegal a c
      | Set.member c illegalChars = a <> pure c
      | otherwise = a <> mempty


-- | Decode a Nix package name using the provided map. The name is decoded with
-- 'fromText' if it is not in the map.
--
-- Throws: 'LeadingDigit', 'IllegalChar'
getName :: MonadThrow m => HashMap Emacs.Name Name -> Emacs.Name -> m Name
getName nameMap name =
  fromMaybe (fromText name) (pure <$> HashMap.lookup name nameMap)


data ParseNixError = ParseNixError { reason :: Doc }
mkException 'PrettyException ''ParseNixError


instance Pretty ParseNixError where
  pretty ParseNixError {..} = reason


-- | @ReadNamesError@ is thrown by 'readNames' if the Nix expression can be
-- parsed and evaluated, but the resulting value is not a valid map of Emacs
-- names to Nix names.
data ReadNamesError =
  ReadNamesError
  { filePath :: FilePath
  , attr :: Maybe Text
  , reason :: Doc
  }
mkException 'PrettyException ''ReadNamesError

instance Pretty ReadNamesError where
  pretty ReadNamesError {..} =
    Pretty.hsep
    [ (Pretty.bold . Pretty.string) filePath <> Pretty.colon
    , maybe Pretty.empty
      (\d -> (Pretty.bold . Pretty.text . Text.unpack) d <> Pretty.colon)
      attr
    , reason
    ]

-- | Read the map of names from a file, which should contain a Nix expression.
readNames :: FilePath
          -> IO (HashMap Emacs.Name Name)
readNames filename =
  do
    result <- parseNixFileLoc filename
    case result of
      Failure err -> throwM (ParseNixError err)
      Success parsed ->
        do
          time <- getCurrentTime
          let opts = defaultOptions time
          getSet =<< runLazyM opts (normalForm =<< evalExprLoc parsed)
  where
    mapKeys f =
      HashMap.fromList . map (\(k, v) -> (f k, v)) . HashMap.toList

    getSet value =
      case unFix value of
        NVSetF names _ ->
          HashMap.traverseWithKey getBound (mapKeys Emacs.Name names)
        NVConstantF {} -> found "constant"
        NVStrF {} -> found "string"
        NVPathF {} -> found "path"
        NVListF {} -> found "list"
        NVClosureF {} -> found "closure"
        NVBuiltinF {} -> found "builtin"
      where
        found what =
          throwM ReadNamesError
          { filePath = filename
          , attr = Nothing
          , reason =
              Pretty.hsep
              [ "expected"
              , Pretty.bold "set" <> Pretty.comma
              , "but found"
              , (Pretty.bold . Pretty.text) what
              ]
          }

    getBound ename0 value =
      case unFix value of
        NVStrF name _ -> pure Name { fromName = name, ename = ename0 }
        NVSetF {} -> found "set"
        NVConstantF {} -> found "constant"
        NVPathF {} -> found "path"
        NVListF {} -> found "list"
        NVClosureF {} -> found "closure"
        NVBuiltinF {} -> found "builtin"
      where
        found what =
          throwM ReadNamesError
          { filePath = filename
          , attr = Just (Emacs.fromName ename0)
          , reason =
              Pretty.hsep
              [ "expected"
              , Pretty.bold "string" <> Pretty.comma
              , "but found"
              , (Pretty.bold . Pretty.text) what
              ]
          }
