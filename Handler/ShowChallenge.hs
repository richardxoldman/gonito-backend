module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import qualified Data.Text.Lazy          as TL
import           Text.Markdown

import Handler.Extract
import Handler.Shared

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  Just repo <- runDB $ get $ challengePublicRepo challenge
  challengeLayout True challenge (showChallengeWidget challenge repo)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  let repoDir = getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  contents <- readFile readmeFilePath
  challengeLayout False challenge $ toWidget $ markdown def $ TL.fromStrict contents

showChallengeWidget challenge repo = $(widgetFile "show-challenge")


getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR name = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
   (formWidget, formEnctype) <- generateFormPost submissionForm
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR name = do
    ((result, formWidget), formEnctype) <- runFormPost submissionForm
    let submissionData = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just (description, submissionUrl, submissionBranch) = submissionData

    runViewProgress $ (flip msg) "HAHA"

challengeSubmissionWidget formWidget formEnctype challenge = $(widgetFile "challenge-submission")

submissionForm :: Form (Text, Text, Text)
submissionForm = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> areq textField (fieldSettingsLabel MsgSubmissionDescription) Nothing
    <*> areq textField (fieldSettingsLabel MsgSubmissionUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgSubmissionBranch) Nothing



challengeLayout withHeader challenge widget = do
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
