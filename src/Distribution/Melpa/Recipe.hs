{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Aeson
import Data.Aeson.Types
  ( Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject, parseEither )
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#else
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import System.FilePath
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

import Paths_emacs2nix (getDataFileName)

data Recipe = Bzr { url :: Text, commit :: Maybe Text }
            | Git { url :: Text, commit :: Maybe Text, branch :: Maybe Text }
            | GitHub { repo :: Text, commit :: Maybe Text, branch :: Maybe Text }
            | CVS { url :: Text, cvsModule :: Maybe Text }
            | Darcs { url :: Text }
            | Fossil { url :: Text }
            | Hg { url :: Text, commit :: Maybe Text }
            | SVN { url :: Text, commit :: Maybe Text }
            | Wiki { wikiUrl :: Maybe Text }
            deriving Generic

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
  let packageBuildEl = melpaDir </> "package-build.el"
      recipesDir = melpaDir </> "recipes"
  dumpRecipesEl <- getDataFileName "dump-recipes.el"
  let args = [ "--batch"
             , "-l", packageBuildEl
             , "-l", dumpRecipesEl
             , "-f", "dump-recipes-json", recipesDir
             ]
  (_, out, _, _) <- S.runInteractiveProcess "emacs" args Nothing Nothing
  result <- parseEither parseJSON <$> S.parseFromStream json' out
  case result of
    Left err -> do
      let msg = "error reading recipes: " <> T.pack err
      S.write (Just msg) =<< S.encodeUtf8 S.stderr
      return M.empty
    Right recipes -> return recipes
