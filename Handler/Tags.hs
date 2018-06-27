module Handler.Tags where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Yesod.Table as Table

import Handler.TagUtils

getTagsR :: Handler Html
getTagsR = do
  (formWidget, formEnctype) <- generateFormPost tagForm
  doTags formWidget formEnctype

postTagsR :: Handler Html
postTagsR = do
  ((result, formWidget), formEnctype) <- runFormPost tagForm
  canTagsBeAdded <- canAddTags
  when canTagsBeAdded $ do
     case result of
      FormSuccess (t, d) -> do
                            _ <- runDB $ insert $ Tag t d
                            return ()
      _ -> do
           return ()
  doTags formWidget formEnctype


canAddTags :: Handler Bool
canAddTags = do
  mUser <- maybeAuth

  app <- getYesod
  let tagPermissions = appTagPermissions $ appSettings app

  case tagPermissions of
    OnlyAdminCanAddNewTags -> return $ checkIfAdmin mUser
    EverybodyCanAddNewTags -> return $ isJust mUser


doTags formWidget formEnctype = do
  tags <- runDB $ selectList [] [Asc TagName]
  canTagsBeAdded <- canAddTags
  defaultLayout $ do
    setTitle "Tags"
    $(widgetFile "tags")

tagsTable :: Table.Table App (Entity Tag)
tagsTable = mempty
  ++ Table.text "tag" (\(Entity _ tag) -> tagName tag)
  ++ Table.text "description" (\(Entity _ tag) -> (fromMaybe (""::Text) (tagDescription tag)))


tagForm :: Form (Text, Maybe Text)
tagForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgTagName) Nothing
    <*> aopt textField (bfs MsgTagDescription) Nothing

getToggleSubmissionTagR :: SubmissionTagId -> Handler RepPlain
getToggleSubmissionTagR submissionTagId = do
  mUser <- maybeAuth
  if (checkIfAdmin mUser)
   then
      do
       submissionTag <- runDB $ get404 submissionTagId
       let newState = toggleTag $ submissionTagAccepted submissionTag
       runDB $ update submissionTagId [SubmissionTagAccepted =. newState]
       return $ RepPlain $ toContent $ tagClass newState
   else
      do
       return $ RepPlain $ toContent ("BLOCKED" :: Text)
