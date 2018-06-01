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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Nix.Name
  ( Name (..)
  , readNames
  , lookupName, getName
  , InvalidName (..)
  , ReadNamesError (..)
  , ParseNixError (..)
  ) where

import Control.Exception
import qualified Data.Char as Char
import Data.Fix ( Fix (..) )
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import qualified Data.Set as Set
import Data.Text ( Text )
import qualified Data.Text as Text
import Data.Time.Clock ( getCurrentTime )
import Data.Typeable
import Nix.Exec ( evalExprLoc, runLazyM )
import Nix.Normal ( normalForm )
import Nix.Options ( defaultOptions )
import Nix.Parser
import Nix.Value
import Text.PrettyPrint.ANSI.Leijen ( Doc, Pretty (..) )
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import qualified Distribution.Emacs.Name as Emacs


-- | A valid Nix package name.
newtype Name = Name { fromName :: Text }
  deriving (Eq, Hashable, Ord, Show)


data InvalidName
  = InvalidNameBeginDigit Text
    -- ^ The name is invalid because it begins with a digit.
  | InvalidNameIllegalChar Text Char
    -- ^ The name is invalid because it contains an illegal character
  deriving (Show, Typeable)

instance Exception InvalidName


-- | Decode a valid Nix package name from 'Text', or if the name is invalid,
-- indicate the violation.
lookupName :: HashMap Emacs.Name Name -> Emacs.Name -> Either InvalidName Name
lookupName nameMap name =
  maybe defaultName Right (HashMap.lookup name nameMap)
  where
    txt = Emacs.fromName name

    defaultName
      | Char.isDigit (Text.head txt) = Left (InvalidNameBeginDigit txt)
      | otherwise =
          case getFirst (Text.foldl' firstIllegal mempty txt) of
            Nothing -> Right (Name txt)
            Just illegal -> Left (InvalidNameIllegalChar txt illegal)

    illegalChars = Set.fromList ['@', '+'] -- may not appear in a Nix name

    -- | Find the first illegal character in the name.
    firstIllegal a c
      | Set.member c illegalChars = a <> pure c
      | otherwise = a <> mempty


-- | Decode a valid Nix package name from 'Text', or if the name is invalid,
-- throw an exception indicating the violation.
getName :: HashMap Emacs.Name Name -> Emacs.Name -> IO Name
getName namesMap name = either throwIO pure (lookupName namesMap name)


data ParseNixError = ParseNixError { parseNixErrorReason :: Doc }
  deriving (Show, Typeable)

instance Exception ParseNixError


-- | @ReadNamesError@ is thrown by 'readNames' if the Nix expression can be
-- parsed and evaluated, but the resulting value is not a valid map of Emacs
-- names to Nix names.
data ReadNamesError =
  ReadNamesError
  { readNamesErrorFilePath :: FilePath
  , readNamesErrorAttr :: Maybe Text
  , readNamesErrorReason :: Doc
  }
  deriving (Show, Typeable)

instance Exception ReadNamesError

instance Pretty ReadNamesError where
  pretty e =
    Pretty.hsep
    [ (Pretty.bold . Pretty.string) (readNamesErrorFilePath e) <> Pretty.colon
    , maybe Pretty.empty
      (\d -> (Pretty.bold . Pretty.text . Text.unpack) d <> Pretty.colon)
      (readNamesErrorAttr e)
    , readNamesErrorReason e
    ]

-- | Read the map of names from a file, which should contain a Nix expression.
readNames :: FilePath
          -> IO (HashMap Emacs.Name Name)
readNames filename =
  do
    result <- parseNixFileLoc filename
    case result of
      Failure err -> throwIO (ParseNixError err)
      Success parsed ->
        do
          time <- getCurrentTime
          let opts = defaultOptions time
          getSet <$> runLazyM opts (normalForm =<< evalExprLoc parsed)
  where
    mapKeys f =
      HashMap.fromList . map (\(k, v) -> (f k, v)) . HashMap.toList

    getSet value =
      case unFix value of
        NVSetF names _ ->
          HashMap.mapWithKey getBound (mapKeys Emacs.Name names)
        NVConstantF {} -> found "constant"
        NVStrF {} -> found "string"
        NVPathF {} -> found "path"
        NVListF {} -> found "list"
        NVClosureF {} -> found "closure"
        NVBuiltinF {} -> found "builtin"
      where
        found what =
          throw ReadNamesError
          { readNamesErrorFilePath = filename
          , readNamesErrorAttr = Nothing
          , readNamesErrorReason =
              Pretty.hsep
              [ "expected"
              , Pretty.bold "set" <> Pretty.comma
              , "but found"
              , (Pretty.bold . Pretty.text) what
              ]
          }

    getBound (Emacs.fromName -> emacsName) value =
      case unFix value of
        NVStrF name _ -> Name name
        NVSetF {} -> found "set"
        NVConstantF {} -> found "constant"
        NVPathF {} -> found "path"
        NVListF {} -> found "list"
        NVClosureF {} -> found "closure"
        NVBuiltinF {} -> found "builtin"
      where
        found what =
          throw ReadNamesError
          { readNamesErrorFilePath = filename
          , readNamesErrorAttr = Just emacsName
          , readNamesErrorReason =
              Pretty.hsep
              [ "expected"
              , Pretty.bold "string" <> Pretty.comma
              , "but found"
              , (Pretty.bold . Pretty.text) what
              ]
          }
