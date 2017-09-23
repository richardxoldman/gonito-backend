module Handler.Home where

import Import

import Handler.Shared

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  maybeUser <- maybeAuth
  master <- getYesod
  let maybeLocalId = case maybeUser of
        Just user -> userLocalId $ entityVal user
        Nothing -> Nothing
  if maybe False ((\u -> isNothing (userLocalId u) && isNothing (userName u)) . entityVal) maybeUser
    then
     do
      setMessage $ toHtml ("First, set up your name and/or ID!" :: Text)
      redirect $ YourAccountR
    else
      defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Gonito.net!"
        $(widgetFile "homepage")
