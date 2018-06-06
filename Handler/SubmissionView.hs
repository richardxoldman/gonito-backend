module Handler.SubmissionView where

import Import
import Handler.Shared
import PersistSHA1

import Data.Text as T(pack)

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmissionId :: SubmissionId,
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo,
  fsiChallenge :: Challenge,
  fsiChallengeRepo :: Repo,
  fsiScheme :: RepoScheme}

getFullInfo :: Entity Submission -> Handler FullSubmissionInfo
getFullInfo (Entity submissionId submission) = do
  repo <- runDB $ get404 $ submissionRepo submission
  user <- runDB $ get404 $ submissionSubmitter submission
  challenge <- runDB $ get404 $ submissionChallenge submission
  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  return $ FullSubmissionInfo {
    fsiSubmissionId = submissionId,
    fsiSubmission = submission,
    fsiUser = user,
    fsiRepo = repo,
    fsiChallenge = challenge,
    fsiChallengeRepo = challengeRepo,
    fsiScheme = scheme}


queryResult submission = do
  $(widgetFile "query-result")
    where commitSha1AsText = fromSHA1ToText $ submissionCommit $ fsiSubmission submission
          submitter = formatSubmitter $ fsiUser submission
          publicSubmissionBranch = getPublicSubmissionBranch $ fsiSubmissionId submission
          publicSubmissionRepo = getReadOnlySubmissionUrl (fsiScheme submission) (fsiChallengeRepo submission) $ challengeName $ fsiChallenge submission
          browsableUrl = browsableGitRepoBranch (fsiScheme submission) (fsiChallengeRepo submission) (challengeName $ fsiChallenge submission) publicSubmissionBranch
          stamp = T.pack $ show $ submissionStamp $ fsiSubmission submission

getTags submissionId = do
  sts <- selectList [SubmissionTagSubmission ==. submissionId] []
  let tagIds = Import.map (submissionTagTag . entityVal) sts
  tags <- mapM get404 $ tagIds
  let tagEnts = Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds tags
  return $ zip tagEnts sts
