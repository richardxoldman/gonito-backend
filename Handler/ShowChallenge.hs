module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import qualified Data.Text.Lazy          as TL
import           Text.Markdown

import System.Directory (doesFileExist)
import qualified Data.Text as T

import Handler.Extract
import Handler.Shared

import PersistSHA1

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
doCreateSubmission challengeId description url branch chan = do
  maybeRepoKey <- getSubmissionRepo challengeId url branch chan
  case maybeRepoKey of
    Just repoId -> do
      repo <- runDB $ get404 repoId
      submissionId <- getSubmission repoId (repoCurrentCommit repo) challengeId description chan
      msg chan "HAHA"
    Nothing -> return ()

getSubmission :: Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission repoId commit challengeId description chan = do
  maybeSubmission <- runDB $ getBy $ UniqueSubmissionRepoCommitChallenge repoId commit challengeId
  case maybeSubmission of
    Just (Entity submissionId submission) -> do
      msg chan "Submission already there, re-checking"
      return submissionId
    Nothing -> do
      msg chan "Creating new submission"
      time <- liftIO getCurrentTime
      runDB $ insert $ Submission {
        submissionRepo=repoId,
        submissionCommit=commit,
        submissionChallenge=challengeId,
        submissionDescription=description,
        submissionStamp=time }

getOuts :: Key Submission -> Handler ([Out])
getOuts submissionId = do
  submission <- runDB $ get404 submissionId
  let challengeId = submissionChallenge submission
  let repoDir = getRepoDir $ submissionRepo submission
  activeTests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  testsDone <- filterM (doesOutExist repoDir) activeTests
  outs <- mapM (outForTest repoDir submissionId) testsDone
  mapM_ checkOrInsertOut outs
  return outs

outFileName = "out.tsv"

getOutFilePath repoDir test = repoDir </> (T.unpack $ testName test) </> outFileName

doesOutExist repoDir (Entity _ test) = liftIO $ doesFileExist $ getOutFilePath repoDir test

outForTest repoDir submissionId (Entity testId test) = do
  checksum <- liftIO $ gatherSHA1ForCollectionOfFiles [getOutFilePath repoDir test]
  return Out {
    outSubmission=submissionId,
    outTest=testId,
    outChecksum=SHA1 checksum }

checkOrInsertOut :: Out -> Handler ()
checkOrInsertOut out = do
  maybeOut <- runDB $ getBy $ UniqueOutSubmissionTestChecksum (outSubmission out) (outTest out) (outChecksum out)
  case maybeOut of
    Just _ -> return ()
    Nothing -> (runDB $ insert out) >> return ()

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
