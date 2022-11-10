module Handler.Extract where

import Import hiding (replace)

import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Shared (stringify)

import System.IO (readFile)
import Data.String.Utils (strip, split)
import Data.List.Utils (replace)

extractHeaders :: Block -> [String]
extractHeaders (Header 1 _ x) = [unpack $ stringify x]
extractHeaders _ = []

extractFirstHeader :: Pandoc -> Maybe String
extractFirstHeader doc = case queryWith extractHeaders doc of
  (s:_) -> Just s
  [] -> Nothing

extractParas :: Block -> [String]
extractParas (Para x) = [unpack $ stringify x]
extractParas _ = []

extractFirstPara :: Pandoc -> Maybe String
extractFirstPara doc = case queryWith extractParas doc of
  (s:_) -> Just s
  [] -> Nothing

readDoc :: String -> Pandoc
readDoc s = case runPure $ readMarkdown def (T.pack s) of
                Right doc -> doc
                Left err  -> error (show err)

defaultTitle :: String
defaultTitle = "[???]"

defaultDescription :: String
defaultDescription = ""

readmeFile :: FilePath
readmeFile = "README.md"

imageFile :: FilePath
imageFile = ".seeme.png"

tagsMarker :: String
tagsMarker = "Tags:"

extractTags :: Pandoc -> [String]
extractTags doc = case filter (tagsMarker `isPrefixOf`) $ queryWith extractParas doc of
                    [] -> []
                    (fpt:_) -> map strip $ split "," $ replace tagsMarker "" fpt

getTitleDescriptionAndTags :: String -> (String, String, [String])
getTitleDescriptionAndTags contents = (title, description, tags)
                       where title = fromMaybe defaultTitle $ extractFirstHeader doc
                             description = fromMaybe defaultDescription $ extractFirstPara doc
                             tags = extractTags doc
                             doc = readDoc contents

extractTitleDescriptionAndTags :: FilePath -> IO (String, String, [String])
extractTitleDescriptionAndTags fp = do
  contents <- System.IO.readFile fp
  return $ getTitleDescriptionAndTags contents
