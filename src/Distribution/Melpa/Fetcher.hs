module Distribution.Melpa.Fetcher where

import Distribution.Melpa.Fetcher.Bzr.Types (Bzr)
import Distribution.Melpa.Fetcher.CVS.Types (CVS)
import Distribution.Melpa.Fetcher.Darcs.Types (Darcs)
import Distribution.Melpa.Fetcher.Fossil.Types (Fossil)
import Distribution.Melpa.Fetcher.Git.Types (Git)
import Distribution.Melpa.Fetcher.GitHub.Types (GitHub)
import Distribution.Melpa.Fetcher.Hg.Types (Hg)
import Distribution.Melpa.Fetcher.SVN.Types (SVN)
import Distribution.Melpa.Fetcher.Wiki.Types (Wiki)

data Fetcher
  = Git !Git
  | GitHub !GitHub
  | Bzr !Bzr
  | Hg !Hg
  | Darcs !Darcs
  | Fossil !Fossil
  | SVN !SVN
  | CVS !CVS
  | Wiki !Wiki
  deriving (Eq, Read, Show)
