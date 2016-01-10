module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Data.Monoid

import qualified Data.Text.Lazy          as TL
import           Text.Markdown

import System.Directory (doesFileExist)
import qualified Data.Text as T

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import Handler.Extract
import Handler.Shared
import Handler.Tables

import GEval.Core
import GEval.OptionsParser

import Data.Map (Map)
import qualified Data.Map as Map

import PersistSHA1

import Options.Applicative

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  Just repo <- runDB $ get $ challengePublicRepo challenge
  leaderboard <- getLeaderboardEntries challengeId
  challengeLayout True challenge (showChallengeWidget challenge repo leaderboard)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  contents <- readFile readmeFilePath
  challengeLayout False challenge $ toWidget $ markdown def $ TL.fromStrict contents

showChallengeWidget challenge repo leaderboard = $(widgetFile "show-challenge")
  where leaderboardWithRanks = zip [1..] leaderboard
        maybeRepoLink = getRepoLink repo


getRepoLink :: Repo -> Maybe Text
getRepoLink repo
  | sitePrefix `isPrefixOf` url = Just $ (browsableGitRepo bareRepoName) ++ "/" ++ (repoBranch repo)
  | otherwise = Nothing
  where sitePrefix = "git://gonito.net/" :: Text
        sitePrefixLen = length sitePrefix
        url = repoUrl repo
        bareRepoName = drop sitePrefixLen url

getChallengeHowToR :: Text -> Handler Html
getChallengeHowToR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  maybeUser <- maybeAuth
  challengeLayout False challenge (challengeHowTo challenge (idToBeShown challenge maybeUser))

idToBeShown challenge maybeUser =
  case maybeUser of
   Just user ->  case userLocalId $ entityVal user of
                 Just localId -> localId
                 Nothing -> defaultIdToBe
   Nothing -> defaultIdToBe
  where defaultIdToBe = "YOURID" :: Text

defaultRepo challenge maybeUser = "ssh://gitolite@gonito.net/" ++ (idToBeShown challenge maybeUser) ++ "/" ++ (challengeName challenge)

challengeHowTo challenge idToBeShown = $(widgetFile "challenge-how-to")

getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR name = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
   maybeUser <- maybeAuth
   (formWidget, formEnctype) <- generateFormPost $ submissionForm (Just $ defaultRepo challenge maybeUser)
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR name = do
    (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
    ((result, formWidget), formEnctype) <- runFormPost $ submissionForm Nothing
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
      _ <- getOuts chan submissionId
      msg chan "Done"
    Nothing -> return ()

getSubmission :: Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission repoId commit challengeId description chan = do
  maybeSubmission <- runDB $ getBy $ UniqueSubmissionRepoCommitChallenge repoId commit challengeId
  userId <- requireAuthId
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
        submissionStamp=time,
        submissionSubmitter=userId }

getOuts :: Channel -> Key Submission -> Handler ([Out])
getOuts chan submissionId = do
  submission <- runDB $ get404 submissionId
  let challengeId = submissionChallenge submission
  repoDir <- getRepoDir $ submissionRepo submission
  activeTests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  testsDone <- filterM (doesOutExist repoDir) activeTests
  outs <- mapM (outForTest repoDir submissionId) testsDone
  mapM_ checkOrInsertOut outs
  mapM_ (checkOrInsertEvaluation repoDir chan) outs
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

checkOrInsertEvaluation :: FilePath -> Channel -> Out -> Handler ()
checkOrInsertEvaluation repoDir chan out = do
  test <- runDB $ get404 $ outTest out
  challenge <- runDB $ get404 $ testChallenge test
  maybeEvaluation <- runDB $ getBy $ UniqueEvaluationTestChecksum (outTest out) (outChecksum out)
  case maybeEvaluation of
    Just (Entity _ evaluation) -> do
      msg chan $ concat ["Already evaluated with score ", (T.pack $ fromMaybe "???" $ show <$> evaluationScore evaluation)]
    Nothing -> do
      msg chan $ "Start evaluation..."
      challengeDir <- getRepoDir $ challengePrivateRepo challenge
      resultOrException <- liftIO $ rawEval challengeDir repoDir (testName test)
      case resultOrException of
        Right (Left parseResult) -> do
          err chan "Cannot parse options, check the challenge repo"
        Right (Right (opts, Just result)) -> do
          msg chan $ concat [ "Evaluated! Score ", (T.pack $ show result) ]
          time <- liftIO getCurrentTime
          runDB $ insert $ Evaluation {
            evaluationTest=outTest out,
            evaluationChecksum=outChecksum out,
            evaluationScore=Just result,
            evaluationErrorMessage=Nothing,
            evaluationStamp=time }
          msg chan "Evaluation done"
        Right (Right (_, Nothing)) -> do
          err chan "Error during the evaluation"
        Left exception -> do
          err chan $ "Evaluation failed: " ++ (T.pack $ show exception)

rawEval :: FilePath -> FilePath -> Text -> IO (Either GEvalException (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe MetricValue)))
rawEval challengeDir repoDir name = try (runGEvalGetOptions [
                                    "--expected-directory", challengeDir,
                                    "--out-directory", repoDir,
                                    "--test-name", (T.unpack name)])

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
    Nothing -> do
      challenge <- runDB $ get404 challengeId
      let repoId = challengePublicRepo challenge
      repo <- runDB $ get404 repoId
      repoDir <- getRepoDir repoId
      cloneRepo' url branch (T.pack repoDir) (repoBranch repo) chan

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

submissionForm :: Maybe Text -> Form (Text, Text, Text)
submissionForm defaultUrl = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> areq textField (fieldSettingsLabel MsgSubmissionDescription) Nothing
    <*> areq textField (fieldSettingsLabel MsgSubmissionUrl) defaultUrl
    <*> areq textField (fieldSettingsLabel MsgSubmissionBranch) (Just "master")

getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR name = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) name

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR name = getChallengeSubmissions (\_ -> True) name

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition name = do
  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  (evaluationMaps, tests) <- getChallengeSubmissionInfos condition challengeId
  challengeLayout True challenge (challengeAllSubmissionsWidget challenge evaluationMaps tests)

challengeAllSubmissionsWidget challenge submissions tests = $(widgetFile "challenge-all-submissions")

challengeLayout withHeader challenge widget = do
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
