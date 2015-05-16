{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Package where

import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Text (Text)

import Distribution.Melpa.Fetcher
import Distribution.Melpa.Recipe (Recipe(Recipe))
import qualified Distribution.Melpa.Recipe as Recipe
import Distribution.Melpa.Version

data Package =
  forall f. (FromJSON f, ToJSON f) => Package
  { ver :: Version
  , deps :: [Text]
  , fetcher :: Fetcher f
  , recipe :: f
  , rev :: Text
  , hash :: Text
  }

instance FromJSON Package where
  parseJSON = withObject "package" $ \obj -> do
    ver <- obj .: "ver"
    deps <- obj .: "deps"
    rcp <- obj .: "recipe"
    rev <- obj .: "rev"
    hash <- obj .: "hash"
    case rcp of
      Recipe { Recipe.recipe = recipe, Recipe.fetcher = fetcher } ->
        return Package {..}

instance ToJSON Package where
  toJSON Package {..} =
    object
    [ "ver" .= ver
    , "deps" .= deps
    , "recipe" .= Recipe { Recipe.fetcher = fetcher, Recipe.recipe = recipe }
    , "rev" .= rev
    , "hash" .= hash
    ]

getPackage :: Text -> Recipe -> IO (Either Text Package)
getPackage = undefined
