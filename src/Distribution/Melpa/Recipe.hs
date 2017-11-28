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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

import Control.Exception ( bracket )
import Data.Aeson
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject, parseEither )
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Paths_emacs2nix (getDataFileName)

data Recipe = Bzr { url :: Text, commit :: Maybe Text }
            | Git { url :: Text, commit :: Maybe Text, branch :: Maybe Text }
            | GitHub { repo :: Text, commit :: Maybe Text, branch :: Maybe Text }
            | GitLab { repo :: Text, commit :: Maybe Text, branch :: Maybe Text }
            | CVS { url :: Text, cvsModule :: Maybe Text }
            | Darcs { url :: Text }
            | Fossil { url :: Text }
            | Hg { url :: Text, commit :: Maybe Text }
            | Bitbucket { repo :: Text, commit :: Maybe Text }
            | SVN { url :: Text, commit :: Maybe Text }
            | Wiki { wikiUrl :: Maybe Text }
            deriving (Eq, Generic, Read, Show)

recipeOptions :: Options
recipeOptions = defaultOptions
                { omitNothingFields = True
                , fieldLabelModifier = recipeFieldModifier
                , constructorTagModifier = recipeTagModifier
                , sumEncoding = defaultTaggedObject { tagFieldName = "fetcher" }
                }
  where
    recipeFieldModifier field =
      case field of
        "cvsModule" -> "module"
        "wikiUrl" -> "url"
        _ -> field
    recipeTagModifier = map Char.toLower

instance ToJSON Recipe where
  toJSON = genericToJSON recipeOptions

instance FromJSON Recipe where
  parseJSON = genericParseJSON recipeOptions

readRecipes :: FilePath -> IO (Map Text Recipe)
readRecipes melpaDir = do
  let packageBuildDir = melpaDir </> "package-build"
      packageBuildEl = "package-build.el"
      recipesDir = melpaDir </> "recipes"
  dumpRecipesEl <- getDataFileName "dump-recipes.el"
  let args = [ "-Q"
             , "--batch"
             , "-L", packageBuildDir
             , "-l", packageBuildEl
             , "-l", dumpRecipesEl
             , "-f", "dump-recipes-json", recipesDir
             ]
  bracket
    (S.runInteractiveProcess "emacs" args Nothing Nothing)
    (\(_, _, _, pid) -> S.waitForProcess pid)
    (\(_, out, _, _) -> do
         result <- parseEither parseJSON <$> S.parseFromStream json' out
         case result of
           Left err -> do
             let msg = "error reading recipes: " <> T.pack err
             S.write (Just msg) =<< S.encodeUtf8 S.stderr
             return M.empty
           Right recipes -> return recipes)
