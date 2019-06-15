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

extrasTable :: Table.Table App ExtraPoints
extrasTable = mempty
  ++ Table.text "reason" extraPointsDescription
  ++ timestampCell "added" extraPointsPosted
  ++ Table.int "points" extraPointsPoints

getStatus :: (AchievementInfo, (Entity Submission, Bool)) -> Text
getStatus (_, (_, False)) = ""
getStatus (aInfo, (_, True)) = T.pack $ show $ achievementInfoPoints aInfo

getPoints :: (AchievementInfo, (Entity Submission, Bool)) -> Int
getPoints (_, (_, False)) = 0
getPoints (aInfo, (_, True)) = achievementInfoPoints aInfo

doScore :: Entity User -> Handler Html
doScore userEnt@(Entity userId user) = do
  courses <- runDB $ selectList [CourseClosed ==. False] [Asc CourseName]

  courseUserInfos <- mapM (userScoreForCourse userEnt) courses

  let courseInfos = Import.filter (\(_, (points, _, _)) -> points > 0) $ Import.zip courses courseUserInfos

  defaultLayout $ do
    setTitle "Score"
    $(widgetFile "score")

scoreForCourse (points, entries, extras) = $(widgetFile "score-for-course")

userScoreForCourse :: Entity User -> Entity Course -> Handler (Int, [(AchievementInfo, (Entity Submission, Bool))], [ExtraPoints])
userScoreForCourse userEnt@(Entity userId user) courseEnt@(Entity courseId course) = do
  achievementEntries <- userAchievementsForCourse userEnt courseId
  let achievementTotal = sum $ Import.map getPoints achievementEntries

  extraEntries <- userExtraPointsForCourse userId courseId
  let extraTotal = sum $ Import.map extraPointsPoints extraEntries

  let total = achievementTotal + extraTotal

  return (total, achievementEntries, extraEntries)

userExtraPointsForCourse :: UserId -> CourseId -> Handler [ExtraPoints]
userExtraPointsForCourse userId courseId = do
  entries <- runDB $ selectList [ExtraPointsUser ==. userId, ExtraPointsCourse ==. courseId] [Asc ExtraPointsPosted]
  return $ Import.map entityVal entries

userAchievementsForCourse :: Entity User -> CourseId -> Handler [(AchievementInfo, (Entity Submission, Bool))]
userAchievementsForCourse (Entity userId user) courseId = do
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

courseSummary :: Entity Course -> Widget
courseSummary entCourse@(Entity courseId course) = do
  students <- handlerToWidget $
               runDB $ E.select
                     $ E.from $ \(participant, user) -> do
                       E.where_ (participant ^. ParticipantCourse E.==. E.val courseId
                                 E.&&. participant ^. ParticipantUser E.==. user ^. UserId)
                       E.orderBy [E.asc (user ^. UserIdent)]
                       return user

  scores <- mapM (handlerToWidget . ((flip userScoreForCourse) entCourse)) students
  $(widgetFile "course-summary")

getCoursesITeachR :: Handler Html
getCoursesITeachR = do
  (Entity userId _) <- requireAuth
  teacherCourses <- runDB $ selectList [TeacherUser ==. userId] []
  let coursesIds = Import.map (teacherCourse . entityVal) teacherCourses
  courses <- runDB $ mapM get404 coursesIds
  let entCourses' = Import.map (\(k, v) -> Entity k v) $ Import.zip coursesIds courses

  let entCourses = sortBy (\e1 e2 -> (courseName $ entityVal e1) `compare`  (courseName $ entityVal e2)) entCourses'

  defaultLayout $ do
    setTitle "Courses I teach"
    $(widgetFile "courses-i-teach")
