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


module Distribution.Wiki ( revision ) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Maybe ( fromMaybe )
import Data.Semigroup ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy ( Text )
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as Text.Lazy
import qualified Data.Text.Read as Text
import qualified Network.Http.Client as HTTP
import Text.Taggy.Parser ( taggyWith )
import Text.Taggy.Types ( Tag (..), Attribute(..) )


defaultURL :: Text -> Maybe Integer -> Text
defaultURL name rev =
  let
    query = (\r -> "?revision=" <> Text.pack (show r)) <$> rev
  in
    "https://www.emacswiki.org/emacs/download/" <> name <> ".el" <> fromMaybe "" query


-- | Find the latest revision of an EmacsWiki file. The revision is a permanent
-- URL referring to a specific version of the file.
revision :: Text -> Maybe Text -> IO Text
revision _ (Just url) = pure url
revision name Nothing =
  do
    body <- getAsText revisionsPageUrl
    return $ defaultURL name $ findLatestRevision $ taggyWith True body
    where
        revisionsPageUrl :: ByteString
        revisionsPageUrl = Text.encodeUtf8 $ "https://www.emacswiki.org/emacs?action=history;id=" <> name <> ".el"

        defaultRevisionAttr = Attribute "href" (defaultURL name Nothing)

        getAsText :: ByteString -> IO Lazy.Text
        getAsText url = Text.Lazy.decodeUtf8 . ByteString.Lazy.fromStrict <$> HTTP.get url HTTP.concatHandler

        readDecimal :: Text -> Maybe Integer
        readDecimal aText =
          case Text.decimal aText of
            Left _ -> Nothing
            Right (i, _) -> Just i

        revisionPrefix = "Revision "

        findLatestRevision [] = Nothing
        findLatestRevision (TagOpen "a" attrs _ : TagText aText : tags)
          | elem defaultRevisionAttr attrs && Text.isPrefixOf revisionPrefix aText =
              readDecimal $ Text.drop (Text.length revisionPrefix) aText
          | otherwise = findLatestRevision tags
        findLatestRevision (_:tags) = findLatestRevision tags
