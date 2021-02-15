module Handler.SubmissionView where

import Import hiding (fromList)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Handler.Shared
import PersistSHA1

import Data.Swagger hiding (get)
import Control.Lens hiding ((.=), (^.))
import Data.Proxy as DPR
import Data.HashMap.Strict.InsOrd (fromList)

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmissionId :: SubmissionId,
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo,
  fsiChallenge :: Challenge,
  fsiChallengeRepo :: Repo,
  fsiScheme :: RepoScheme,
  fsiTags :: [(Entity Import.Tag, Entity SubmissionTag)],
  fsiExternalLinks :: [Entity ExternalLink],
  fsiSuperSubmissions :: [FullSubmissionInfo] }

instance ToJSON FullSubmissionInfo where
  toJSON entry = object
        [ "hash" .= (fromSHA1ToText $ submissionCommit $ fsiSubmission entry),
          "submitter" .= (formatSubmitter $ fsiUser entry),
          "challenge" .= (challengeName $ fsiChallenge entry)
        ]

instance ToSchema FullSubmissionInfo where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    return $ NamedSchema (Just "SubmissionInfo") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("hash", stringSchema)
                     , ("submitter", stringSchema)
                     , ("challenge", stringSchema)
                    ]
        & required .~ [ "hash", "submitter", "challenge" ]



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

getTags :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) => Key Submission -> ReaderT backend m [(Entity Import.Tag, Entity SubmissionTag)]
getTags submissionId = do
  sts <- selectList [SubmissionTagSubmission ==. submissionId] []
  let tagIds = Import.map (submissionTagTag . entityVal) sts
  tags <- mapM get404 $ tagIds
  let tagEnts = Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds tags
  return $ zip tagEnts sts
