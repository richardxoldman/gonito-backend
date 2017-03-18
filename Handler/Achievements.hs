module Handler.Achievements where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils

import Handler.Tables
import Handler.Shared

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Text

import qualified Yesod.Table as Table

data AchievementInfo = AchievementInfo {
  achievementInfoName :: Text,
  achievementInfoDescription :: Maybe Text,
  achievementInfoPoints :: Int,
  achievementInfoDeadline :: UTCTime,
  achievementInfoMaxWinners :: Maybe Int,
  achievementInfoWorkingOn :: [Entity User],
  achievementInfoCurrentUser :: Maybe (Entity User),
  achievementInfoTags :: [Entity Tag] }



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
      FormSuccess (name, description, points, deadlineDay, deadlineTime, maxSubmitters, mTags) -> do
                            -- @TODO for the time being hardcoded
                            Just challengeEnt <- runDB $ getBy $ UniqueName "petite-difference-challenge2"

                            achievementId <- runDB $ insert $ Achievement name (entityKey challengeEnt) points description (UTCTime { utctDay = deadlineDay, utctDayTime = timeOfDayToTime deadlineTime }) maxSubmitters

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

getAchievementInfo mUser (Entity achievementId achievement) = do
  es <- selectList [WorkingOnAchievement ==. achievementId] []
  let userIds = Import.map (workingOnUser . entityVal) es
  users <- mapM get404 userIds

  tags <- getAchievementTags achievementId

  return $ AchievementInfo {
    achievementInfoName = achievementName achievement,
    achievementInfoDescription = achievementDescription achievement,
    achievementInfoPoints = achievementPoints achievement,
    achievementInfoDeadline = achievementDeadline achievement,
    achievementInfoMaxWinners = achievementMaxWinners achievement,
    achievementInfoWorkingOn = Import.map (\(i,v) -> Entity i v) $ Import.zip userIds users,
    achievementInfoCurrentUser = mUser,
    achievementInfoTags = tags }

getAchievementTags achievementId = do
  sts <- selectList [AchievementTagAchievement ==. achievementId] []
  let tagIds = Import.map (achievementTagTag . entityVal) sts
  tags <- mapM get404 $ tagIds
  return $ Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds tags


achievementsTable :: Table.Table App (AchievementInfo)
achievementsTable = mempty
  ++ Table.text "achievement" achievementInfoName
  ++ achievementDescriptionCell
  ++ Table.int "points" achievementInfoPoints
  ++ timestampCell "deadline" achievementInfoDeadline
  ++ Table.string "max submitters" (formatMaxSubmitters . achievementInfoMaxWinners)
  ++ Table.text "who's working on it?" (formatSubmitters . achievementInfoWorkingOn)

achievementDescriptionCell = Table.widget "description" (
  \ainfo -> fragmentWithTags (fromMaybe (""::Text) $ achievementInfoDescription ainfo) (achievementInfoTags ainfo))

formatSubmitters userEnts = Data.Text.intercalate ", " $ Import.map (formatSubmitter . entityVal) userEnts

formatMaxSubmitters :: Maybe Int -> String
formatMaxSubmitters Nothing = "no limit"
formatMaxSubmitters (Just m) = show m

achievementForm :: Form (Text, Maybe Text, Int, Day, TimeOfDay, Maybe Int, Maybe Text)
achievementForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> areq textField (bfs MsgAchievementName) Nothing
    <*> aopt textField (bfs MsgAchievementDescription) Nothing
    <*> areq intField (bfs MsgAchievementPoints) Nothing
    <*> areq dayField (bfs MsgAchievementDeadlineDay) Nothing
    <*> areq timeFieldTypeTime (bfs MsgAchievementDeadlineTime) Nothing
    <*> aopt intField (bfs MsgAchievementMaxWinners) Nothing
    <*> aopt textField (tagsfs MsgAchievementTags) Nothing
