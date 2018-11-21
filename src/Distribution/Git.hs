
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

module Distribution.Git ( revision ) where

import Data.Maybe ( maybeToList )
import Data.Semigroup ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified System.IO.Streams as Stream

import Exceptions
import Process ( runInteractiveProcess )

-- | Find the latest revision in a Git repository.
--
-- The revision will be the latest one which changes the given files
-- (or which changes /any/ file, if no file names are given).
--
-- If the branch name is given, the revision will be the latest in that branch.
-- Otherwise, the latest revision of the current branch will be found.
revision :: FilePath  -- ^ repository
         -> Maybe Text  -- ^ branch name
         -> [FilePath]  -- ^ file names
         -> IO Text
revision src branch files =
  do
    runInteractiveProcess "git" args (Just src) Nothing $ \out -> do
      revs <- Stream.lines out >>= Stream.decodeUtf8
      maybe (throwM NoRevision) pure =<< Stream.read revs
  where
    args = [ "log", "--first-parent", "-n1", "--pretty=format:%H" ]
           -- package-build does not fetch all branches by default, so they
           -- must be referred to under the origin/ prefix
           ++ maybeToList (Text.unpack . (<>) "origin/" <$> branch)
           ++ ("--" : files)
