{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Name
       ( Name
       , fromText, fromName
       ) where

import Data.Char ( isDigit )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.ICU.Replace ( replaceAll )

import Distribution.Nix.Pretty

newtype Name = Name { fromName :: Text }

instance Pretty Name where
  pretty = text . fromName

fromText :: Text -> Name
fromText = Name . prefixNumerals . replaceAll "@" "at"
  where
    prefixNumerals txt
      | T.null txt = txt
      | isDigit (T.head txt) = T.cons '_' txt
      | otherwise = txt
