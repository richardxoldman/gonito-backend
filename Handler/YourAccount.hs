module Handler.YourAccount where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Handler.Shared
import Handler.Extract

getYourAccountR :: Handler Html
getYourAccountR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId
    (formWidget, formEnctype) <- generateFormPost (yourAccountForm $ userName user)
    let submission = Nothing :: Maybe (Import.FileInfo, Text)
        handlerName = "getYourAccountR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Your account"
        $(widgetFile "your-account")

postYourAccountR :: Handler Html
postYourAccountR = do
    ((result, formWidget), formEnctype) <- runFormPost (yourAccountForm Nothing)
    let handlerName = "postYourAccountR" :: Text
        accountData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (name, aboutMe) = accountData
    userId <- requireAuthId
    runDB $ update userId [UserName =. name]
    user <- runDB $ get404 userId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Your account"
        $(widgetFile "your-account")


yourAccountForm :: Maybe Text -> Form (Maybe Text, Maybe Text)
yourAccountForm maybeName = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> aopt textField (fieldSettingsLabel MsgAccountName) (Just maybeName)
    <*> aopt textField (fieldSettingsLabel MsgAboutMe) Nothing
