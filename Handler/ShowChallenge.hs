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
    (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
    ((result, formWidget), formEnctype) <- runFormPost submissionForm
    let submissionData = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just (description, submissionUrl, submissionBranch) = submissionData

    runViewProgress $ doCreateSubmission challengeId description submissionUrl submissionBranch

doCreateSubmission :: Key Challenge -> Text -> Text -> Text -> Channel -> Handler ()
doCreateSubmission challengeId _ url branch chan = do
  maybeRepoKey <- getSubmissionRepo challengeId url branch chan
  case maybeRepoKey of
    Just repoId -> do
      repo <- runDB $ get404 repoId
      msg chan "HAHA"
    Nothing -> return ()

getSubmissionRepo :: Key Challenge -> Text -> Text -> Channel -> Handler (Maybe (Key Repo))
getSubmissionRepo challengeId url branch chan = do
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  case maybeRepo of
    Just (Entity repoId repo) -> do
      msg chan "Repo already there"
      available <- checkRepoAvailibility challengeId repoId chan
      if available
         then
          do
           updateStatus <- updateRepo repoId chan
           if updateStatus
             then
               return $ Just repoId
             else
               return Nothing
         else
           return Nothing
    Nothing -> cloneRepo' url branch chan


checkRepoAvailibility :: Key Challenge -> Key Repo -> Channel -> Handler Bool
checkRepoAvailibility challengeId repoId chan = do
  maybeOtherChallengeId <- runDB $ selectFirst ( [ChallengePublicRepo ==. repoId]
                                                 ||. [ChallengePrivateRepo ==. repoId]) []
  case maybeOtherChallengeId of
    Just _ -> do
      err chan "Repository already used as a challenge repo, please use a different repo or a different branch"
      return False
    Nothing -> do
      maybeOtherSubmissionId <- runDB $ selectFirst [SubmissionRepo ==. repoId,
                                                    SubmissionChallenge !=. challengeId] []
      case maybeOtherSubmissionId of
        Just _ -> do
          err chan "Repository already used as a submission repo for a different challenge, please use a different repo or a different branch"
          return False
        Nothing -> return True


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
