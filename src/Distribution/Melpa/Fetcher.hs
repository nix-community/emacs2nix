module Distribution.Melpa.Fetcher where

import Data.Text (Text)

class Fetchable f where
  addCommit :: Text -> f -> f
