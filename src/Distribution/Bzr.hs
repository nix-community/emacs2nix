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


module Distribution.Bzr ( revision ) where

import qualified Data.Char as Char
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified System.IO.Streams as Stream

import Exceptions
import Process ( runInteractiveProcess )

-- | Find the latest revision in a Bazaar repository.
revision :: FilePath -> IO Text
revision tmp =
  do
    let args = [ "log", "-l1", tmp ]
    runInteractiveProcess "bzr" args Nothing Nothing $ \out -> do
      revs <- Stream.mapMaybe revno =<< (Stream.lines out >>= Stream.decodeUtf8)
      maybe (throwM NoRevision) pure =<< Stream.read revs
  where
    revno = (Text.takeWhile Char.isDigit . Text.strip <$>) . Text.stripPrefix "revno:"
