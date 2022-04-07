module Handler.Achievements where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils

import Handler.Tables
import Handler.Shared

import Handler.AchievementUtils

import Data.Time.LocalTime

import Data.Text

import Gonito.ExtractMetadata (parseTags)

import qualified Yesod.Table as Table

getGonitoInClassR :: Handler Html
getGonitoInClassR = do
  defaultLayout $ do
    setTitle "Achievements"
    $(widgetFile "gonito-in-class")

getAchievementsR :: Handler Html
getAchievementsR = do
  (formWidget, formEnctype) <- generateFormPost (achievementForm Nothing Nothing)
  mUser <- maybeAuth
  doAchievements mUser formWidget formEnctype

postAchievementsR :: Handler Html
postAchievementsR = do
  ((result, formWidget), formEnctype) <- runFormPost (achievementForm Nothing Nothing)
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (name, description, points, deadlineDay, deadlineTime, maxSubmitters, mTags, challengeId, courseId) -> do
                            achievementId <- runDB $ insert $ Achievement name challengeId points description (UTCTime { utctDay = deadlineDay, utctDayTime = timeOfDayToTime deadlineTime }) maxSubmitters courseId

                            tids <- runDB $ tagsAsTextToTagIds (parseTags mTags)

                            _ <- mapM (\tid -> runDB $ insert $ AchievementTag achievementId tid) tids

                            return ()
      _ -> do
           return ()
  doAchievements mUser formWidget formEnctype

doAchievements mUser formWidget formEnctype = do
  achievements <- runDB $ selectList [] [Asc AchievementName]
  achievementInfos'' <- runDB $ mapM (getAchievementInfo mUser) achievements
  let achievementInfos' = Import.filter (not . courseClosed . entityVal . achievementInfoCourse) achievementInfos''

  courses <- case mUser of
    Just (Entity userId _) -> do
      participantEnts <- runDB $ selectList [ParticipantUser ==. userId] []
      teacherEnts <- runDB $ selectList [TeacherUser ==. userId] []
      return $ (Import.map (participantCourse . entityVal) participantEnts) ++
               (Import.map (teacherCourse . entityVal) teacherEnts)
    Nothing -> do
      return []

  let achievementInfos = Import.filter (isParticipant courses) achievementInfos'

  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON

  defaultLayout $ do
    setTitle "Achievements"
    $(widgetFile "achievements")

isParticipant :: [CourseId] -> AchievementInfo -> Bool
isParticipant [] _ = True
isParticipant courses info = (entityKey $ achievementInfoCourse info) `elem` courses

achievementsTable :: Bool -> Table.Table App (AchievementInfo)
achievementsTable canEdit = mempty
  ++ achievementNameEntry canEdit
  ++ Table.text "course" (courseName . entityVal . achievementInfoCourse)
  ++ Table.linked "challenge" (challengeTitle . entityVal . achievementInfoChallenge) (ShowChallengeR . challengeName . entityVal . achievementInfoChallenge)
  ++ achievementDescriptionCell id
  ++ Table.int "points" achievementInfoPoints
  ++ timestampCell "deadline" achievementInfoDeadline
  ++ Table.string "max submitters" (formatMaxSubmitters . achievementInfoMaxWinners)
  ++ workingOnCell

achievementNameEntry :: Bool -> Table.Table App AchievementInfo
achievementNameEntry True = Table.linked "achievement" (achievementInfoName) (EditAchievementR . achievementInfoId)
achievementNameEntry False = Table.text "achievement" achievementInfoName

workingOnCell :: Table.Table App AchievementInfo
workingOnCell = Table.widget "who's working on it?" workingOnWidget

workingOnWidget :: AchievementInfo -> WidgetFor App ()
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
   (Entity userId _) <- requireAuth
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

  achievement <- runDB $ get404 achievementId
  let courseId = achievementCourse achievement

  alreadyWorkingOn <- runDB $ selectList [WorkingOnUser ==. userId, WorkingOnFinalSubmission ==. Nothing] []
  achievementsWorkingOn <- runDB $ mapM (get404 . workingOnAchievement . entityVal) alreadyWorkingOn
  let achievementsWorkingOnInTheSameCourse = Import.filter (\a -> achievementCourse a == courseId) achievementsWorkingOn

  if Import.null achievementsWorkingOnInTheSameCourse
    then
       do
        es <- runDB $ selectList [WorkingOnAchievement ==. achievementId] []
        let userIds = Import.map (workingOnUser . entityVal) es
        users <- runDB $ mapM get404 userIds
        let userEnts = Import.map (\(k,v) -> (Entity k v)) $ Import.zip userIds users

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
        setMessage $ toHtml ("Already working on another achievement in the same course!" :: Text)
  redirect $ AchievementsR


getGiveUpWorkingOnR :: AchievementId -> Handler Html
getGiveUpWorkingOnR achievementId = do
  (Entity userId _) <- requireAuth

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
determineWhetherCanStartWorkingOn (Just (Entity userId _)) peopleWorkingOn maxWinners =
  (Import.all (\e -> (userId /= entityKey e)) peopleWorkingOn) && (checkLimit peopleWorkingOn maxWinners)

determineWhetherCanGiveUpWorkingOn Nothing _ = False
determineWhetherCanGiveUpWorkingOn (Just (Entity userId _)) peopleWorkingOn =
  (Import.any (\e -> (userId == entityKey e)) peopleWorkingOn)

checkLimit _ Nothing = True
checkLimit peopleWorkingOn (Just m) = (Import.length peopleWorkingOn) < m

formatSubmitters :: [Entity User] -> Text
formatSubmitters userEnts = Data.Text.intercalate ", " $ Import.map (formatSubmitter . entityVal) userEnts

formatMaxSubmitters :: Maybe Int -> String
formatMaxSubmitters Nothing = "no limit"
formatMaxSubmitters (Just m) = show m

achievementForm :: Maybe Achievement -> Maybe [Entity Tag] -> Form (Text, Maybe Text, Int, Day, TimeOfDay, Maybe Int, Maybe Text, ChallengeId, CourseId)
achievementForm mAchievement mTags = renderBootstrap3 BootstrapBasicForm $ (,,,,,,,,)
    <$> areq textField (bfs MsgAchievementName) (achievementName <$> mAchievement)
    <*> aopt textField (bfs MsgAchievementDescription) (achievementDescription <$> mAchievement)
    <*> areq intField (bfs MsgAchievementPoints) (achievementPoints <$> mAchievement)
    <*> areq dayField (bfs MsgAchievementDeadlineDay) (utctDay <$> achievementDeadline <$> mAchievement)
    <*> areq timeFieldTypeTime (bfs MsgAchievementDeadlineTime) (timeToTimeOfDay <$> utctDayTime <$> achievementDeadline <$> mAchievement)
    <*> aopt intField (bfs MsgAchievementMaxWinners) (achievementMaxWinners <$> mAchievement)
    <*> aopt textField (tagsfs MsgAchievementTags) (tagsToText <$> mTags)
    <*> challengesSelectFieldList (achievementChallenge <$> mAchievement)
    <*> coursesSelectFieldList (achievementCourse <$> mAchievement)

tagsToText :: [Entity Tag] -> Maybe Text
tagsToText [] = Nothing
tagsToText tags = Just $ Data.Text.intercalate ", " $ Import.map (tagName . entityVal) tags

challengesSelectFieldList mChallengeId = areq (selectField challenges) (bfs MsgChallenge) mChallengeId
    where
      challenges = do
        challengeEnts <- runDB $ selectList [] [Asc ChallengeTitle]
        optionsPairs $ Import.map (\ch -> (challengeTitle $ entityVal ch, entityKey ch)) challengeEnts


coursesSelectFieldList mCourseId = areq (selectField courses) (bfs MsgCourse) mCourseId
    where
      courses = do
        courseEnts <- runDB $ selectList [] [Asc CourseName]
        optionsPairs $ Import.map (\ch -> (courseName $ entityVal ch, entityKey ch)) courseEnts

getEditAchievementR :: AchievementId -> Handler Html
getEditAchievementR achievementId = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  achievement <- runDB $ get404 achievementId
  tags <- runDB $ getAchievementTags achievementId
  (formWidget, formEnctype) <- generateFormPost (achievementForm (Just achievement) (Just tags))
  mUser <- maybeAuth

  defaultLayout $ do
    setTitle "Edit achievements"
    $(widgetFile "edit-achievement")

postEditAchievementR :: AchievementId -> Handler Html
postEditAchievementR achievementId = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  ((result, formWidget), formEnctype) <- runFormPost (achievementForm Nothing Nothing)
  mUser <- maybeAuth

  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (name, description, points, deadlineDay, deadlineTime, maxSubmitters, mTags, challengeId, courseId) -> do
        runDB $ do
          update achievementId [AchievementName =. name,
                                AchievementDescription =. description,
                                AchievementPoints =. points,
                                AchievementDeadline =. UTCTime { utctDay = deadlineDay,
                                                                 utctDayTime = timeOfDayToTime deadlineTime },
                                AchievementMaxWinners =. maxSubmitters,
                                AchievementChallenge =. challengeId,
                                AchievementCourse =. courseId]

          deleteWhere [AchievementTagAchievement ==. achievementId]
          tids <- tagsAsTextToTagIds (parseTags mTags)
          mapM (\tid -> insert $ AchievementTag achievementId tid) tids

        setMessage $ toHtml ("OK! Achievement modified" :: Text)
        return ()
      _ -> do
           return ()


  defaultLayout $ do
    setTitle "Edit achievements"
    $(widgetFile "edit-achievement")
