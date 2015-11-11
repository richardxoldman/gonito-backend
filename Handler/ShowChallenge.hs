module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Data.Monoid
import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Data.Text.Lazy          as TL
import           Text.Markdown

import System.Directory (doesFileExist)
import qualified Data.Text as T

import Handler.Extract
import Handler.Shared

import GEval.Core
import GEval.OptionsParser

import Data.Map (Map)
import qualified Data.Map as Map

import PersistSHA1

import Options.Applicative

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
  let repoDir = getRepoDir $ submissionRepo submission
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
      resultOrException <- liftIO $ rawEval challenge repoDir
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

rawEval :: Challenge -> FilePath -> IO (Either GEvalException (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe MetricValue)))
rawEval challenge repoDir = try (runGEvalGetOptions [
                                    "--expected-directory", (getRepoDir $ challengePrivateRepo challenge),
                                    "--out-directory", repoDir])

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
      cloneRepo' url branch (T.pack $ getRepoDir repoId) (repoBranch repo) chan

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

getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR name = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) name

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR name = getChallengeSubmissions (\_ -> True) name

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  allSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId] [Desc SubmissionStamp]
  let submissions = filter condition allSubmissions
  tests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  evaluationMaps <- mapM getEvaluationMap submissions
  challengeLayout True challenge (challengeAllSubmissionsWidget challenge evaluationMaps tests)



getEvaluationMap :: Entity Submission -> Handler (Entity Submission, User, Map (Key Test) Evaluation)
getEvaluationMap s@(Entity submissionId submission) = do
  outs <- runDB $ selectList [OutSubmission ==. submissionId] []
  user <- runDB $ get404 $ submissionSubmitter submission
  maybeEvaluations <- runDB $ mapM (\(Entity _ o) -> getBy $ UniqueEvaluationTestChecksum (outTest o) (outChecksum o)) outs
  let evaluations = catMaybes maybeEvaluations
  let m = Map.fromList $ map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  return (s, user, m)

challengeAllSubmissionsWidget challenge submissions tests = $(widgetFile "challenge-all-submissions")

submissionsTable :: [Entity Test] -> Table site (Entity Submission, User, Map (Key Test) Evaluation)
submissionsTable tests = mempty
  ++ Table.text "submitter" (formatSubmitter . \(_, submitter, _) -> submitter)
  ++ Table.string "when" (show . submissionStamp . \(Entity _ s, _, _) -> s)
  ++ Table.text "description" (submissionDescription . \(Entity _ s, _,  _) -> s)
  ++ mconcat (map (\(Entity k t) -> Table.string (testName t) (submissionScore k)) tests)

formatSubmitter :: User -> Text
formatSubmitter user = case userName user of
  Just name -> name
  Nothing -> "[name not given]"

submissionScore :: Key Test -> (Entity Submission, User, Map (Key Test) Evaluation) -> String
submissionScore k (_, _, m) = fromMaybe "N/A" (presentScore <$> lookup k m)

presentScore :: Evaluation -> String
presentScore evaluation = fromMaybe "???" (show <$> evaluationScore evaluation)

challengeLayout withHeader challenge widget = do
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
