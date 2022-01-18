module Handler.Announcements where

import Import

import Handler.Shared
import Web.Announcements (sendAnnouncement,
                          AnnouncementPiece(..),
                          AnnouncementMessage,
                          AnnouncementHook,
                          toAnnouncementHook)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

sendChallengeAnnouncement :: ChallengeId -> AnnouncementMessage -> Handler ()
sendChallengeAnnouncement challengeId announcementMsg = do
  hooks <- fetchChallengeHooks challengeId
  liftIO $ mapM_ (\hook -> sendAnnouncement hook announcementMsg) hooks

fetchChallengeHooks :: ChallengeId -> Handler [AnnouncementHook]
fetchChallengeHooks challengeId = do
  courses <- runDB $ E.select $ E.from $ \(course, course_challenge) -> do
    E.where_ (course_challenge ^. CourseChallengeChallenge E.==. E.val challengeId
              E.&&. course_challenge ^. CourseChallengeCourse E.==. course ^. CourseId)
    return course

  case catMaybes $ map (courseAnnouncementHook . entityVal) courses of
    [] -> do
      app <- getYesod
      let mWebHook = appAnnouncementHook $ appSettings app
      return $ maybeToList mWebHook
    hooks -> return $ map toAnnouncementHook hooks

getTestChallengeAnnouncementsR :: Text -> Handler Html
getTestChallengeAnnouncementsR challengeName = do
  (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName

  app <- getYesod
  sendChallengeAnnouncement challengeId (testMessage app)

  defaultLayout $ do
    setTitle "Test announcements"
    $(widgetFile "test-challenge-announcements")

getTestAnnouncementsR :: Handler Html
getTestAnnouncementsR = do
  app <- getYesod

  let webHook = appAnnouncementHook $ appSettings app

  case webHook of
    Just hook -> liftIO $ sendAnnouncement hook (testMessage app)
    Nothing -> return ()

  defaultLayout $ do
    setTitle "Test announcements"
    $(widgetFile "test-announcements")

testMessage :: App -> AnnouncementMessage
testMessage app = [AnnouncementText "Test message from ",
                   (linkInAnnouncement app name "")]
  where name = case appLocation $ appSettings app of
                 Just loc -> "Gonito@" ++ loc
                 Nothing -> "Gonito"
