module Handler.ShowChallenge where

import Import

import qualified Data.Text.Lazy          as TL
import           Text.Markdown

import Handler.Extract
import Handler.Shared

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  challengeLayout True challenge (showChallengeWidget challenge)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  let repoDir = getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  contents <- readFile readmeFilePath
  challengeLayout False challenge $ toWidget $ markdown def $ TL.fromStrict contents

showChallengeWidget challenge = $(widgetFile "show-challenge")

challengeLayout withHeader challenge widget = do
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
