module Handler.SubmissionView where

import Import

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmissionId :: SubmissionId,
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo,
  fsiChallenge :: Challenge,
  fsiChallengeRepo :: Repo,
  fsiScheme :: RepoScheme,
  fsiTags :: [(Entity Tag, Entity SubmissionTag)],
  fsiExternalLinks :: [Entity ExternalLink],
  fsiSuperSubmissions :: [FullSubmissionInfo] }

getFullInfo :: Entity Submission -> Handler FullSubmissionInfo
getFullInfo (Entity submissionId submission) = do
  repo <- runDB $ get404 $ submissionRepo submission
  user <- runDB $ get404 $ submissionSubmitter submission
  challenge <- runDB $ get404 $ submissionChallenge submission
  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  tags <- runDB $ getTags submissionId

  links <- runDB $ selectList [ExternalLinkSubmission ==. submissionId] [Asc ExternalLinkTitle]

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  superSubmissions <- runDB $ E.select $ E.from $ \(submission', dependency) -> do
                              E.where_ (submission' ^. SubmissionCommit E.==. dependency ^. DependencySuperRepoCommit
                                        E.&&. dependency ^. DependencySubRepoCommit E.==. (E.val (submissionCommit submission)))
                              return submission'

  superSubmissionFsis <- mapM getFullInfo superSubmissions

  return $ FullSubmissionInfo {
    fsiSubmissionId = submissionId,
    fsiSubmission = submission,
    fsiUser = user,
    fsiRepo = repo,
    fsiChallenge = challenge,
    fsiChallengeRepo = challengeRepo,
    fsiScheme = scheme,
    fsiTags = tags,
    fsiExternalLinks = links,
    fsiSuperSubmissions = superSubmissionFsis }

getTags :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) => Key Submission -> ReaderT backend m [(Entity Tag, Entity SubmissionTag)]
getTags submissionId = do
  sts <- selectList [SubmissionTagSubmission ==. submissionId] []
  let tagIds = Import.map (submissionTagTag . entityVal) sts
  tags <- mapM get404 $ tagIds
  let tagEnts = Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds tags
  return $ zip tagEnts sts
