module Handler.Score where

import Import

import Handler.Shared
import Handler.Tables

import Handler.AchievementUtils

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import qualified Yesod.Table as Table

getMyScoreR :: Handler Html
getMyScoreR = do
  entUser <- requireAuth
  doScore entUser

getScoreR :: UserId -> Handler Html
getScoreR userId = do
  user <- runDB $ get404 userId
  doScore (Entity userId user)

scoreTable :: Table.Table App (AchievementInfo, Entity Submission)
scoreTable = mempty
  ++ Table.text "name" (achievementInfoName . fst)
  ++ achievementDescriptionCell fst
  ++ timestampCell "deadline" (achievementInfoDeadline . fst)
  ++ Table.text "submission" (submissionDescription . entityVal . snd)

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

processEntry :: Entity User -> (Entity Achievement, Entity Submission) -> Handler (AchievementInfo, Entity Submission)
processEntry entUser (entAchievement, entSubmission) = do
  aInfo <- runDB $ getAchievementInfo (Just entUser) entAchievement
  return (aInfo, entSubmission)
