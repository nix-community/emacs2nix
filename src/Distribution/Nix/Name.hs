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

{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Name
       ( Name
       , fromText, fromName
       ) where

import Data.Char ( isDigit )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.ICU.Replace ( replaceAll )

newtype Name = Name { fromName :: Text }
  deriving (Eq, Ord)

fromText :: Text -> Name
fromText = Name
           . prefixDigits

           . replaceAll "@" "-at-"
           . replaceAll "^@" "at-"
           . replaceAll "@$" "-at"
           . replaceAll "^@$" "at"

           . replaceAll "\\+" "-plus-"
           . replaceAll "^\\+" "plus-"
           . replaceAll "\\+$" "-plus"
           . replaceAll "^\\+$" "plus"
  where
    -- Nix does not allow identifiers to begin with digits
    prefixDigits txt
      | T.null txt = txt
      | isDigit (T.head txt) = T.cons '_' txt
      | otherwise = txt
