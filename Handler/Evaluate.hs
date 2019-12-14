module Handler.Evaluate where

import Import

import Handler.Common
import Handler.Runner
import Handler.Shared

canBeReevaluated :: (YesodAuthPersist (HandlerSite m), MonadHandler m, PersistUniqueRead backend, AuthEntity (HandlerSite m) ~ User, AuthId (HandlerSite m) ~ Key User, BaseBackend backend ~ SqlBackend) => Key Submission -> ReaderT backend m Bool
canBeReevaluated submissionId = do
  maybeUser <- maybeAuth
  case maybeUser of
    Just (Entity userId _) -> do
      isOwner <- checkWhetherGivenUserRepo userId submissionId
      let isSuperuser = checkIfAdmin maybeUser

      submission <- get404 submissionId
      let submissionVersionHash = submissionVersion submission

      challenge <- get404 $ submissionChallenge submission
      let challengeVersionHash = challengeVersion challenge

      if (submissionVersionHash == challengeVersionHash)
       then return False
       else
        do
         (Entity _ submissionVer) <- getBy404 $ UniqueVersionByCommit submissionVersionHash
         (Entity _ chalengeVer) <- getBy404 $ UniqueVersionByCommit challengeVersionHash

         return  ((isOwner || isSuperuser)
                  &&
                  ((versionMajor submissionVer) == (versionMajor chalengeVer)
                  || (versionMinor submissionVer) == (versionMinor chalengeVer)
                  || (versionPatch submissionVer) < (versionPatch chalengeVer)))


    Nothing -> return False

getReevaluateSubmissionR :: SubmissionId -> Handler TypedContent
getReevaluateSubmissionR submissionId =
  runViewProgress $ doReevaluateSubmission submissionId

doReevaluateSubmission :: SubmissionId -> Channel -> Handler ()
doReevaluateSubmission submissionId chan = do
  status <- runDB $ canBeReevaluated submissionId
  if status
    then
      msg chan "Will re-evaluate!"
    else
      msg chan "Won't re-evaluate!"
