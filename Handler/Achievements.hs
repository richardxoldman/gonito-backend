module Handler.Achievements where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils

import Handler.Tables
import Handler.Shared

import Handler.AchievementUtils

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Text

import qualified Yesod.Table as Table

getAchievementsR :: Handler Html
getAchievementsR = do
  (formWidget, formEnctype) <- generateFormPost achievementForm
  mUser <- maybeAuth
  doAchievements mUser formWidget formEnctype

postAchievementsR :: Handler Html
postAchievementsR = do
  ((result, formWidget), formEnctype) <- runFormPost achievementForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (name, description, points, deadlineDay, deadlineTime, maxSubmitters, mTags, challengeId, courseId) -> do
                            achievementId <- runDB $ insert $ Achievement name challengeId points description (UTCTime { utctDay = deadlineDay, utctDayTime = timeOfDayToTime deadlineTime }) maxSubmitters courseId

                            tids <- runDB $ tagsAsTextToTagIds mTags

                            _ <- mapM (\tid -> runDB $ insert $ AchievementTag achievementId tid) tids

                            return ()
      _ -> do
           return ()
  doAchievements mUser formWidget formEnctype

doAchievements mUser formWidget formEnctype = do
  achievements <- runDB $ selectList [] [Asc AchievementName]
  mUser <- maybeAuth
  achievementInfos <- runDB $ mapM (getAchievementInfo mUser) achievements

  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON

  defaultLayout $ do
    setTitle "Achievements"
    $(widgetFile "achievements")

achievementsTable :: Table.Table App (AchievementInfo)
achievementsTable = mempty
  ++ Table.text "achievement" achievementInfoName
  ++ Table.text "course" (courseName . entityVal . achievementInfoCourse)
  ++ Table.linked "challenge" (challengeTitle . entityVal . achievementInfoChallenge) (ShowChallengeR . challengeName . entityVal . achievementInfoChallenge)
  ++ achievementDescriptionCell id
  ++ Table.int "points" achievementInfoPoints
  ++ timestampCell "deadline" achievementInfoDeadline
  ++ Table.string "max submitters" (formatMaxSubmitters . achievementInfoMaxWinners)
  ++ workingOnCell

workingOnCell = Table.widget "who's working on it?" workingOnWidget

workingOnWidget ainfo = [whamlet|
#{srs}

$if canStartWorkingOn
   \ <a href=@{StartWorkingOnR (achievementInfoId ainfo)}>start working</a>
$if canGiveUpWorkingOn
   \ <a href=@{GiveUpWorkingOnR (achievementInfoId ainfo)}>give up</a>
|]
  where srs = formatSubmitters $ achievementInfoWorkingOn ainfo
        canStartWorkingOn = determineWhetherCanStartWorkingOn (achievementInfoCurrentUser ainfo) (achievementInfoWorkingOn ainfo) (achievementInfoMaxWinners ainfo)
        canGiveUpWorkingOn = determineWhetherCanGiveUpWorkingOn (achievementInfoCurrentUser ainfo) (achievementInfoWorkingOn ainfo)

getSubmissionForAchievementR :: SubmissionId -> WorkingOnId -> Handler Html
getSubmissionForAchievementR submissionId workingOnId = do
   (Entity userId user) <- requireAuth
   submission <- runDB $ get404 submissionId
   workingOn <- runDB $ get404 workingOnId
   if submissionSubmitter submission == userId && workingOnUser workingOn == userId
     then
       do
        runDB $ update workingOnId [WorkingOnFinalSubmission =. Just submissionId]
        setMessage $ toHtml ("OK! Your submission now awaits being accepted by a human reviewer" :: Text)
     else
       do
        setMessage $ toHtml ("Not your submission" :: Text)
   redirect $ EditSubmissionR submissionId

getStartWorkingOnR :: AchievementId -> Handler Html
getStartWorkingOnR achievementId = do
  (Entity userId user) <- requireAuth

  alreadyWorkingOn <- runDB $ selectList [WorkingOnUser ==. userId, WorkingOnFinalSubmission ==. Nothing] []
  if Import.null alreadyWorkingOn
    then
       do
        es <- runDB $ selectList [WorkingOnAchievement ==. achievementId] []
        let userIds = Import.map (workingOnUser . entityVal) es
        users <- runDB $ mapM get404 userIds
        let userEnts = Import.map (\(k,v) -> (Entity k v)) $ Import.zip userIds users

        achievement <- runDB $ get404 achievementId

        if determineWhetherCanStartWorkingOn (Just (Entity userId user)) userEnts (achievementMaxWinners achievement)
          then
           do
            _ <- runDB $ insert $ WorkingOn achievementId userId Nothing
            setMessage $ toHtml ("OK!" :: Text)
          else
           do
            setMessage $ toHtml ("Too many people working on the achievement!" :: Text)
    else
       do
        setMessage $ toHtml ("Already working on another achievement!" :: Text)
  redirect $ AchievementsR


getGiveUpWorkingOnR :: AchievementId -> Handler Html
getGiveUpWorkingOnR achievementId = do
  (Entity userId user) <- requireAuth

  alreadyWorkingOn <- runDB $ selectList [WorkingOnUser ==. userId,
                                         WorkingOnAchievement ==. achievementId,
                                         WorkingOnFinalSubmission ==. Nothing] []
  if not (Import.null alreadyWorkingOn)
    then
       do
        runDB $ deleteWhere [WorkingOnUser ==. userId,
                             WorkingOnAchievement ==. achievementId,
                             WorkingOnFinalSubmission ==. Nothing]
        setMessage $ toHtml ("OK, you can take another achievement now!" :: Text)
    else
       do
        setMessage $ toHtml ("Not working on this achievement!" :: Text)
  redirect $ AchievementsR



determineWhetherCanStartWorkingOn Nothing _ _ = False
determineWhetherCanStartWorkingOn (Just (Entity userId user)) peopleWorkingOn maxWinners =
  (Import.all (\e -> (userId /= entityKey e)) peopleWorkingOn) && (checkLimit peopleWorkingOn maxWinners)

determineWhetherCanGiveUpWorkingOn Nothing _ = False
determineWhetherCanGiveUpWorkingOn (Just (Entity userId user)) peopleWorkingOn =
  (Import.any (\e -> (userId == entityKey e)) peopleWorkingOn)

checkLimit _ Nothing = True
checkLimit peopleWorkingOn (Just m) = (Import.length peopleWorkingOn) < m

formatSubmitters userEnts = Data.Text.intercalate ", " $ Import.map (formatSubmitter . entityVal) userEnts

formatMaxSubmitters :: Maybe Int -> String
formatMaxSubmitters Nothing = "no limit"
formatMaxSubmitters (Just m) = show m

achievementForm :: Form (Text, Maybe Text, Int, Day, TimeOfDay, Maybe Int, Maybe Text, ChallengeId, CourseId)
achievementForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,,,)
    <$> areq textField (bfs MsgAchievementName) Nothing
    <*> aopt textField (bfs MsgAchievementDescription) Nothing
    <*> areq intField (bfs MsgAchievementPoints) Nothing
    <*> areq dayField (bfs MsgAchievementDeadlineDay) Nothing
    <*> areq timeFieldTypeTime (bfs MsgAchievementDeadlineTime) Nothing
    <*> aopt intField (bfs MsgAchievementMaxWinners) Nothing
    <*> aopt textField (tagsfs MsgAchievementTags) Nothing
    <*> challengesSelectFieldList
    <*> coursesSelectFieldList

challengesSelectFieldList = areq (selectField challenges) (bfs MsgChallenge) Nothing
    where
      challenges = do
        challengeEnts <- runDB $ selectList [] [Asc ChallengeTitle]
        optionsPairs $ Import.map (\ch -> (challengeTitle $ entityVal ch, entityKey ch)) challengeEnts


coursesSelectFieldList = areq (selectField courses) (bfs MsgCourse) Nothing
    where
      courses = do
        courseEnts <- runDB $ selectList [] [Asc CourseName]
        optionsPairs $ Import.map (\ch -> (courseName $ entityVal ch, entityKey ch)) courseEnts
