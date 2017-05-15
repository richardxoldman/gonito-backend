module Handler.Score where

import Import

import Handler.Shared
import Handler.Tables

import Control.Monad.Extra

import Handler.AchievementUtils

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import qualified Yesod.Table as Table

import Data.Text as T

getMyScoreR :: Handler Html
getMyScoreR = do
  entUser <- requireAuth
  doScore entUser

getScoreR :: UserId -> Handler Html
getScoreR userId = do
  user <- runDB $ get404 userId
  doScore (Entity userId user)

scoreTable :: Table.Table App (AchievementInfo, (Entity Submission, Bool))
scoreTable = mempty
  ++ Table.text "name" (achievementInfoName . fst)
  ++ achievementDescriptionCell fst
  ++ timestampCell "deadline" (achievementInfoDeadline . fst)
  ++ timestampCell "submitted" (submissionStamp . entityVal . fst . snd)
  ++ Table.linked "submission" (submissionDescription . entityVal . fst . snd) (EditSubmissionR . entityKey . fst . snd)
  ++ Table.text "status" getStatus

getStatus :: (AchievementInfo, (Entity Submission, Bool)) -> Text
getStatus (_, (_, False)) = ""
getStatus (aInfo, (_, True)) = T.pack $ show $ achievementInfoPoints aInfo

doScore :: Entity User -> Handler Html
doScore (Entity userId user) = do
  entries <- runDB $ E.select
                     $ E.from $ \(working_on, achievement, submission) -> do
                       E.where_ (working_on ^. WorkingOnAchievement E.==. achievement ^. AchievementId
                                 E.&&. E.just (submission ^. SubmissionId) E.==. working_on ^. WorkingOnFinalSubmission
                                 E.&&. working_on ^. WorkingOnUser E.==. E.val userId)
                       E.orderBy [E.asc (submission ^. SubmissionStamp)]
                       return (achievement, submission)

  entries' <- mapM (processEntry (Entity userId user)) entries

  defaultLayout $ do
    setTitle "Score"
    $(widgetFile "score")

processEntry :: Entity User -> (Entity Achievement, Entity Submission) -> Handler (AchievementInfo, (Entity Submission, Bool))
processEntry entUser (entAchievement, entSubmission) = do
  aInfo <- runDB $ getAchievementInfo (Just entUser) entAchievement

  accepted <- allM (checkSubmissionTag entSubmission) (achievementInfoTags aInfo)

  return (aInfo, (entSubmission, accepted))


checkSubmissionTag :: Entity Submission -> Entity Tag -> Handler Bool
checkSubmissionTag (Entity submissionId _) (Entity tagId _) = do
  mSubmissionTag <- runDB $ getBy $ UniqueSubmissionTag submissionId tagId
  return $ case mSubmissionTag of
    Just (Entity _ submissionTag) -> case submissionTagAccepted submissionTag of
      Just b -> b
      Nothing -> False
    Nothing -> False
