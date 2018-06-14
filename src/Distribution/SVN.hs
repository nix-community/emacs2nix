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


{-# LANGUAGE OverloadedStrings #-}

module Distribution.SVN ( revision ) where

import Data.Text ( Text )
import qualified Data.Text as Text
import qualified System.IO.Streams as Stream

import Exceptions
import Process ( runInteractiveProcess )


-- | Find the latest revision in a Subversion repository.
revision :: FilePath -> IO Text
revision tmp = do
  runInteractiveProcess "svn" ["info"] (Just tmp) Nothing $ \out -> do
    revs <- Stream.mapMaybe getRev =<< (Stream.lines out >>= Stream.decodeUtf8)
    maybe (throwIO NoRevision) pure =<< Stream.read revs
  where
    getRev = (<$>) Text.strip . Text.stripPrefix "Revision:"
