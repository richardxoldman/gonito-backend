module Handler.CreateChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Handler.Shared

getCreateChallengeR :: Handler Html
getCreateChallengeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getCreateChallengeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "create-challenge")

postCreateChallengeR :: Handler TypedContent
postCreateChallengeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postCreateChallengeR" :: Text
        challengeData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (name, publicUrl, publicBranch, privateUrl, privateBranch) = challengeData

    runViewProgress $ doRepoCloning publicUrl publicBranch

sampleForm :: Form (Text, Text, Text, Text, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> areq textField (fieldSettingsLabel MsgName) Nothing
    <*> areq textField (fieldSettingsLabel MsgPublicUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
    <*> areq textField (fieldSettingsLabel MsgPrivateUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
