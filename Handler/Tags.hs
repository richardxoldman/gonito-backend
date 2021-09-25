{-# LANGUAGE OverloadedLists #-}

module Handler.Tags where

import Import hiding (fromList, get)
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Yesod.Table as Table

import Handler.TagUtils

import Data.Swagger.Declare
import Data.Swagger hiding (Tag, tags)
import Data.Proxy as DPR
import Control.Lens hiding ((.=))
import Data.HashMap.Strict.InsOrd (fromList)

instance ToJSON (Entity Tag) where
    toJSON v = object
        [ "name" .= (tagName $ entityVal v)
        , "description" .= (tagDescription $ entityVal v)
        , "color" .= (tagColor $ entityVal v)
        , "id" .= (entityKey v)
        ]

instance ToSchema (Entity Tag) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    intSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Int)
    return $ NamedSchema (Just "Tag") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("description", stringSchema)
                     , ("color", stringSchema)
                     , ("id", intSchema)
                    ]
        & required .~ [ "name", "description", "color", "id" ]

listTagsApi :: Swagger
listTagsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareListTagsSwagger mempty

declareListTagsSwagger :: Declare (Definitions Schema) Swagger
declareListTagsSwagger = do
  listTagsResponse      <- declareResponse (DPR.Proxy :: DPR.Proxy [Entity Tag])

  return $ mempty
    & paths .~
        [ ("/api/list-tags", mempty & get ?~ (mempty
                                              & produces ?~ MimeList ["application/json"]
                                              & description ?~ "Returns the list of all tags"
                                              & at 200 ?~ Inline listTagsResponse))
        ]


getListTagsJsonR :: Handler Value
getListTagsJsonR = do
  allTags <- fetchAllTags
  return $ toJSON allTags

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
      FormSuccess (t, d, c) -> do
                            _ <- runDB $ insert $ Tag t d c
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


fetchAllTags :: (PersistQueryRead (YesodPersistBackend site),
                YesodPersist site,
                BaseBackend (YesodPersistBackend site) ~ SqlBackend)
               => HandlerFor site [Entity Tag]
fetchAllTags = runDB $ selectList [] [Asc TagName]

doTags formWidget formEnctype = do
  tags <- fetchAllTags
  canTagsBeAdded <- canAddTags
  defaultLayout $ do
    setTitle "Tags"
    $(widgetFile "tags")

tagsTable :: Table.Table App (Entity Tag)
tagsTable = mempty
  ++ Table.text "tag" (\(Entity _ tag) -> tagName tag)
  ++ Table.text "description" (\(Entity _ tag) -> (fromMaybe (""::Text) (tagDescription tag)))
  ++ Table.text "color" (\(Entity _ tag) -> (fromMaybe (""::Text) (tagColor tag)))

tagForm :: Form (Text, Maybe Text, Maybe Text)
tagForm = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> areq textField (bfs MsgTagName) Nothing
    <*> aopt textField (bfs MsgTagDescription) Nothing
    <*> aopt textField (bfs MsgColor) Nothing

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
