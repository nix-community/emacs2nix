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

module Distribution.Nix.Builtin (optionalBuiltins) where

import qualified Data.Set as S
import Data.Text (Text)
import Nix.Types

optionalBuiltins :: Text -> (Text, Maybe NExpr)
optionalBuiltins dep
  | S.member dep builtin = (dep, Just (mkSym "null"))
  | otherwise = (dep, Nothing)
  where
    builtin = S.fromList
              [ "allout", "allout-widgets"
              , "ansi-color"
              , "antlr-mode"
              , "artist"
              , "bs"
              , "cedet"
              , "cfengine"
              , "chart"
              , "checkdoc"
              , "cl-lib"
              , "cwarn"
              , "delim-col"
              , "dunnet"
              , "ebnf2ps"
              , "ede"
              , "ediff"
              , "edmacro"
              , "eieio", "eieio-core"
              , "epg"
              , "erc"
              , "ert"
              , "eshell"
              , "feedmail"
              , "find-cmd"
              , "finder"
              , "flymake"
              , "foldout"
              , "footnote"
              , "gamegrid"
              , "gnus"
              , "hippie-exp"
              , "htmlfontify"
              , "icalendar"
              , "idlwave"
              , "image-dired"
              , "info-xref"
              , "inversion"
              , "isearchb"
              , "js", "json"
              , "linum"
              , "master"
              , "md4"
              , "meta-mode"
              , "mh-e"
              , "mixal-mode"
              , "newsticker"
              , "ntlm"
              , "package"
              , "printing"
              , "ps-mode"
              , "ps-print"
              , "pulse"
              , "python"
              , "regi"
              , "remember"
              , "repeat"
              , "ruby-mode"
              , "ruler-mode"
              , "savehist"
              , "semantic"
              , "sh-script"
              , "sql"
              , "srecode"
              , "tabulated-list"
              , "tetris"
              , "thingatpt"
              , "tildify"
              , "timeclock"
              , "url"
              , "vera-mode"
              , "viper"
              , "wdired"
              , "whitespace"
              , "woman"
              ]
