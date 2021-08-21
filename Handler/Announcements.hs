module Handler.Announcements where

import Import

import Handler.Shared
import Web.Announcements (runSlackHook)

getTestAnnouncementsR :: Handler Html
getTestAnnouncementsR = do
  app <- getYesod

  let webHook = appNewBestResultSlackHook $ appSettings app

  let name = case appLocation $ appSettings app of
               Just loc -> "Gonito@" ++ loc
               Nothing -> "Gonito"

  case webHook of
    Just hook -> liftIO $ runSlackHook hook ("Test message from " ++ (slackLink app name ""))
    Nothing -> return ()

  defaultLayout $ do
    setTitle "Test announcements"
    $(widgetFile "test-announcements")
