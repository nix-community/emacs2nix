{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nix.Name where

import Data.Text ( Text )
import Data.Text.ICU.Replace ( replaceAll )

cleanName :: Text -> Text
cleanName = replaceAll "@" "at"
