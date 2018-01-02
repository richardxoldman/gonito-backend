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

getPoints :: (AchievementInfo, (Entity Submission, Bool)) -> Int
getPoints (_, (_, False)) = 0
getPoints (aInfo, (_, True)) = achievementInfoPoints aInfo

doScore :: Entity User -> Handler Html
doScore userEnt@(Entity userId user) = do
  courses <- runDB $ selectList [CourseClosed ==. False] [Asc CourseName]

  courseAchievementInfos <- mapM (userScoreForCourse userEnt) courses

  let courseInfos = Import.filter (\(_, (points, _)) -> points > 0) $ Import.zip courses courseAchievementInfos

  defaultLayout $ do
    setTitle "Score"
    $(widgetFile "score")


userScoreForCourse :: Entity User -> Entity Course -> Handler (Int, [(AchievementInfo, (Entity Submission, Bool))])
userScoreForCourse userEnt courseEnt = do
  achievementEntries <- userAchievementsForCourse userEnt courseEnt

  let achievementTotal = sum $ Import.map getPoints achievementEntries

  return (achievementTotal, achievementEntries)

userAchievementsForCourse :: Entity User -> Entity Course -> Handler [(AchievementInfo, (Entity Submission, Bool))]
userAchievementsForCourse (Entity userId user) (Entity courseId course) = do
  entries <- runDB $ E.select
                     $ E.from $ \(working_on, achievement, submission) -> do
                       E.where_ (working_on ^. WorkingOnAchievement E.==. achievement ^. AchievementId
                                 E.&&. E.just (submission ^. SubmissionId) E.==. working_on ^. WorkingOnFinalSubmission
                                 E.&&. working_on ^. WorkingOnUser E.==. E.val userId
                                 E.&&. achievement ^. AchievementCourse E.==. E.val courseId)
                       E.orderBy [E.asc (submission ^. SubmissionStamp)]
                       return (achievement, submission)

  entries' <- mapM (processEntry (Entity userId user)) entries

  return entries'

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
