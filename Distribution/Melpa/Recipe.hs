{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Melpa.Recipe where

import Control.Applicative
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)

data Recipe
  = Git { url :: String
        , commit :: Maybe String
        , branch :: Maybe String
        , files :: Maybe [FilePath]
        }
  | GitHub { repo :: String
           , commit :: Maybe String
           , branch :: Maybe String
           , files :: Maybe [FilePath]
           }
  | Bzr { url :: String
        , files :: Maybe [FilePath]
        }
  | Hg { url :: String
       , files :: Maybe [FilePath]
       }
  | Darcs { url :: String
          , files :: Maybe [FilePath]
          }
  | Fossil { url :: String
           , files :: Maybe [FilePath]
           }
  | SVN { url :: String
        , files :: Maybe [FilePath]
        }
  | CVS { url :: String
        , branch :: Maybe String
        , files :: Maybe [FilePath]
        }
  | Wiki { wikiurl :: Maybe String }
  deriving (Eq, Ord)

instance FromJSON Recipe where
  parseJSON (Object obj) = do
    fetcher <- obj .: "fetcher"
    case fetcher of
      "git" -> do
        url <- obj .: "url"
        commit <- obj .:? "commit"
        branch <- obj .:? "branch"
        files <- obj .:? "files"
        return Git {..}
      "github" -> do
        repo <- obj .: "repo"
        commit <- obj .:? "commit"
        branch <- obj .:? "branch"
        files <- obj .:? "files"
        return GitHub {..}
      "bzr" -> do
        url <- obj .: "url"
        files <- obj .:? "files"
        return Bzr {..}
      "hg" -> do
        url <- obj .: "url"
        files <- obj .:? "files"
        return Hg {..}
      "darcs" -> do
        url <- obj .: "url"
        files <- obj .:? "files"
        return Darcs {..}
      "fossil" -> do
        url <- obj .: "url"
        files <- obj .:? "files"
        return Fossil {..}
      "svn" -> do
        url <- obj .: "url"
        files <- obj .:? "files"
        return SVN {..}
      "cvs" -> do
        url <- obj .: "url"
        branch <- obj .:? "module"
        files <- obj .:? "files"
        return CVS {..}
      "wiki" -> do
        wikiurl <- obj .:? "url"
        return Wiki {..}
      _ -> fail ("unknown fetcher: " ++ fetcher)
  parseJSON other = fail ("expected object, but got: " ++ show other)

instance ToJSON Recipe where
  toJSON rcp =
    case rcp of
      Git {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "git")
        , Just ("url", toJSON url)
        , (,) "commit" . toJSON <$> commit
        , (,) "branch" . toJSON <$> branch
        , (,) "files" . toJSON <$> files
        ]
      GitHub {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "github")
        , Just ("repo", toJSON repo)
        , (,) "commit" . toJSON <$> commit
        , (,) "branch" . toJSON <$> branch
        , (,) "files" . toJSON <$> files
        ]
      Bzr {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "bzr")
        , Just ("url", toJSON url)
        , (,) "files" . toJSON <$> files
        ]
      Hg {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "hg")
        , Just ("url", toJSON url)
        , (,) "files" . toJSON <$> files
        ]
      Darcs {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "darcs")
        , Just ("url", toJSON url)
        , (,) "files" . toJSON <$> files
        ]
      Fossil {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "fossil")
        , Just ("url", toJSON url)
        , (,) "files" . toJSON <$> files
        ]
      SVN {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "svn")
        , Just ("url", toJSON url)
        , (,) "files" . toJSON <$> files
        ]
      CVS {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "cvs")
        , Just ("url", toJSON url)
        , (,) "module" . toJSON <$> branch
        , (,) "files" . toJSON <$> files
        ]
      Wiki {..} ->
        Object
        $ HM.fromList
        $ catMaybes
        [ Just ("fetcher", "wiki")
        , (,) "url" . toJSON <$> wikiurl
        ]
