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

    runViewProgress $ doCreateChallenge name publicUrl publicBranch privateUrl privateBranch

doCreateChallenge :: Text -> Text -> Text -> Text -> Text -> Channel -> Handler ()
doCreateChallenge name publicUrl publicBranch privateUrl privateBranch chan = do
  maybePublicRepoId <- cloneRepo publicUrl publicBranch chan
  case maybePublicRepoId of
    Just publicRepoId -> do
      maybePrivateRepoId <- cloneRepo privateUrl privateBranch chan
      case maybePrivateRepoId of
          Just privateRepoId -> addChallenge name publicRepoId privateRepoId chan
          Nothing -> return ()
    Nothing -> return ()

addChallenge :: Text -> (Key Repo) -> (Key Repo) -> Channel -> Handler ()
addChallenge name publicRepoId privateRepoId chan = do
  msg chan "adding challenge..."
  time <- liftIO getCurrentTime
  challengeId <- runDB $ insert $ Challenge {
    challengePublicRepo=publicRepoId,
    challengePrivateRepo=privateRepoId,
    challengeName=name,
    challengeTitle="[UNKNOWN TITLE]",
    challengeDescription="[UNKNOWN DESCRIPTION]",
    challengeStamp=time}
  return ()


sampleForm :: Form (Text, Text, Text, Text, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> areq textField (fieldSettingsLabel MsgName) Nothing
    <*> areq textField (fieldSettingsLabel MsgPublicUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
    <*> areq textField (fieldSettingsLabel MsgPrivateUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
