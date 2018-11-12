module Handler.SubmissionView where

import Import
import Handler.Shared
import PersistSHA1
import Handler.TagUtils

import Data.Text as T(pack)

import qualified Yesod.Table as Table
import Yesod.Table (Table)

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmissionId :: SubmissionId,
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo,
  fsiChallenge :: Challenge,
  fsiChallengeRepo :: Repo,
  fsiScheme :: RepoScheme,
  fsiTags :: [(Entity Tag, Entity SubmissionTag)] }

getFullInfo :: Entity Submission -> Handler FullSubmissionInfo
getFullInfo (Entity submissionId submission) = do
  repo <- runDB $ get404 $ submissionRepo submission
  user <- runDB $ get404 $ submissionSubmitter submission
  challenge <- runDB $ get404 $ submissionChallenge submission
  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  tags <- runDB $ getTags submissionId

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  return $ FullSubmissionInfo {
    fsiSubmissionId = submissionId,
    fsiSubmission = submission,
    fsiUser = user,
    fsiRepo = repo,
    fsiChallenge = challenge,
    fsiChallengeRepo = challengeRepo,
    fsiScheme = scheme,
    fsiTags = tags }

getTags submissionId = do
  sts <- selectList [SubmissionTagSubmission ==. submissionId] []
  let tagIds = Import.map (submissionTagTag . entityVal) sts
  tags <- mapM get404 $ tagIds
  let tagEnts = Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds tags
  return $ zip tagEnts sts
