{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Melpa where

import Control.Error hiding (err)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import System.FilePath
import qualified System.IO.Streams as S

import Distribution.Melpa.Archive (readArchive)
import Distribution.Melpa.Package (Package)
import Distribution.Melpa.Recipe (readRecipes)

make :: FilePath -> Bool -> [Text] -> IO ()
make melpa stable args_ = do
  (inp, out, err, pid) <- S.runInteractiveProcess "make" args (Just melpa) Nothing
  S.write Nothing inp
  S.supply out S.stdout
  S.supply err S.stderr
  _ <- S.waitForProcess pid
  return ()
  where args = "SHELL=/bin/sh" : (if stable then ("STABLE=t" :) else id) (map T.unpack args_)

updateMelpa :: FilePath -> FilePath -> Bool -> IO (HashMap Text Package)
updateMelpa melpa nixpkgs stable = undefined
