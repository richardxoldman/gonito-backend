module Handler.Extract where

import Import

import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Shared (stringify)

import System.IO (readFile)

extractHeaders :: Block -> [String]
extractHeaders (Header 1 _ x) = [stringify x]
extractHeaders _ = []

extractFirstHeader :: Pandoc -> Maybe String
extractFirstHeader doc = case queryWith extractHeaders doc of
  (s:_) -> Just s
  [] -> Nothing

extractParas :: Block -> [String]
extractParas (Para x) = [stringify x]
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

getTitleAndDescription :: String -> (String, String)
getTitleAndDescription contents = (title, description)
                       where title = fromMaybe defaultTitle $ extractFirstHeader doc
                             description = fromMaybe defaultDescription $ extractFirstPara doc
                             doc = readDoc contents

extractTitleAndDescription :: FilePath -> IO (String, String)
extractTitleAndDescription fp = do
  contents <- System.IO.readFile fp
  return $ getTitleAndDescription contents
