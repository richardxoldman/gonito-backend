module Handler.Tags where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Yesod.Table as Table

getTagsR :: Handler Html
getTagsR = do
  (formWidget, formEnctype) <- generateFormPost tagForm
  mUser <- maybeAuth
  doTags mUser formWidget formEnctype

postTagsR :: Handler Html
postTagsR = do
  ((result, formWidget), formEnctype) <- runFormPost tagForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (t, d) -> do
                            _ <- runDB $ insert $ Tag t d
                            return ()
      _ -> do
           return ()
  doTags mUser formWidget formEnctype

doTags mUser formWidget formEnctype = do
  tags <- runDB $ selectList [] [Asc TagName]
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
