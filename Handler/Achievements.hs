module Handler.Achievements where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils

import Handler.Tables

import Data.Time.Clock
import Data.Time.LocalTime

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

  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON

  defaultLayout $ do
    setTitle "Achievements"
    $(widgetFile "achievements")

achievementsTable :: Table.Table App (Entity Achievement)
achievementsTable = mempty
  ++ Table.text "achievement" (\(Entity _ achievement) -> achievementName achievement)
  ++ Table.text "description" (\(Entity _ achievement) -> (fromMaybe (""::Text) (achievementDescription achievement)))
  ++ Table.int "points" (\(Entity _ achievement) -> achievementPoints achievement)
  ++ timestampCell "deadline" (\(Entity _ achievement) -> achievementDeadline achievement)

achievementForm :: Form (Text, Maybe Text, Int, Day, TimeOfDay, Maybe Int, Maybe Text)
achievementForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> areq textField (bfs MsgAchievementName) Nothing
    <*> aopt textField (bfs MsgAchievementDescription) Nothing
    <*> areq intField (bfs MsgAchievementPoints) Nothing
    <*> areq dayField (bfs MsgAchievementDeadlineDay) Nothing
    <*> areq timeFieldTypeTime (bfs MsgAchievementDeadlineTime) Nothing
    <*> aopt intField (bfs MsgAchievementMaxWinners) Nothing
    <*> aopt textField (tagsfs MsgAchievementTags) Nothing
