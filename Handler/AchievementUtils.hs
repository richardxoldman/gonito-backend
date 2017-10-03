module Handler.AchievementUtils where

import Import

import Handler.TagUtils

import qualified Yesod.Table as Table

data AchievementInfo = AchievementInfo {
  achievementInfoId :: AchievementId,
  achievementInfoName :: Text,
  achievementInfoCourse :: Entity Course,
  achievementInfoChallenge :: Entity Challenge,
  achievementInfoDescription :: Maybe Text,
  achievementInfoPoints :: Int,
  achievementInfoDeadline :: UTCTime,
  achievementInfoMaxWinners :: Maybe Int,
  achievementInfoWorkingOn :: [Entity User],
  achievementInfoCurrentUser :: Maybe (Entity User),
  achievementInfoTags :: [Entity Tag] }


achievementDescriptionCell fun = Table.widget "description" ((
  \ainfo -> fragmentWithTags (fromMaybe (""::Text) $ achievementInfoDescription ainfo) (achievementInfoTags ainfo)) . fun)

getAchievementInfo mUser (Entity achievementId achievement) = do
  es <- selectList [WorkingOnAchievement ==. achievementId] []
  let userIds = Import.map (workingOnUser . entityVal) es
  users <- mapM get404 userIds

  tags <- getAchievementTags achievementId

  let challengeId = achievementChallenge achievement
  challenge <- get404 challengeId

  let courseId = achievementCourse achievement
  course <- get404 courseId

  return $ AchievementInfo {
    achievementInfoId = achievementId,
    achievementInfoName = achievementName achievement,
    achievementInfoCourse = Entity courseId course,
    achievementInfoChallenge = Entity challengeId challenge,
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
