{-

emacs2nix - Generate Nix expressions for Emacs packages
Copyright (C) 2018 Thomas Tuegel

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

{-# LANGUAGE TemplateHaskell #-}

module System.IO.Streams.Pretty where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified System.IO.Streams as Streams
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import Exceptions

data DisplayException = DisplayException
mkException 'PrettyException ''DisplayException

instance Pretty.Pretty DisplayException where
  pretty DisplayException = "Could not display document!"

displayStream :: Pretty.SimpleDoc -> Streams.OutputStream Text.Text -> IO ()
displayStream sdoc out =
    display sdoc
  where
    indentation i = Text.replicate i (Text.singleton ' ')
    display Pretty.SFail = Exception.throwIO DisplayException
    display Pretty.SEmpty = return ()
    display (Pretty.SChar c sdoc') =
      do
        Streams.write (Just (Text.singleton c)) out
        display sdoc'
    display (Pretty.SText _ t sdoc') =
      do
        Streams.write (Just (Text.pack t)) out
        display sdoc'
    display (Pretty.SLine i sdoc') =
      do
        Streams.write (Just (Text.cons '\n' (indentation i))) out
        display sdoc'
    display (Pretty.SSGR _ sdoc') = display sdoc'
