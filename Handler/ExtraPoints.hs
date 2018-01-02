module Handler.ExtraPoints where

import Import
import Handler.Common (checkIfAdmin)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

getExtraPointsR :: Handler Html
getExtraPointsR = do
  (formWidget, formEnctype) <- generateFormPost extraPointsForm
  defaultLayout $ do
    $(widgetFile "extra-points")

postExtraPointsR :: Handler Html
postExtraPointsR = do
  ((result, formWidget), formEnctype) <- runFormPost extraPointsForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (points, description, userId, courseId) -> do
                            now <- liftIO getCurrentTime
                            let (Just (Entity adderId _)) = mUser
                            _ <- runDB $ insert $ ExtraPoints points description userId courseId now adderId
                            setMessage $ toHtml ("OK! Extra points added" :: Text)
                            return ()
      _ -> do
           return ()
  defaultLayout $ do
    $(widgetFile "extra-points")

extraPointsForm :: Form (Int, Text, UserId, CourseId)
extraPointsForm  = renderBootstrap3 BootstrapBasicForm $ (,,,)
    <$> areq intField (bfs MsgExtraPointsPoints) Nothing
    <*> areq textField (bfs MsgExtraPointsDescription) Nothing
    <*> usersSelectFieldList
    <*> coursesSelectFieldList

usersSelectFieldList = areq (selectField users) (bfs MsgUser) Nothing
    where
      users = do
        userEnts <- runDB $ selectList [] [Asc UserName]
        optionsPairs $ Import.map (\ch -> (userInSelection $ entityVal ch, entityKey ch)) userEnts

userInSelection :: User -> Text
userInSelection user = (fromMaybe "" $ userLocalId user) ++ " / " ++ (fromMaybe "" $ userName user)

coursesSelectFieldList = areq (selectField courses) (bfs MsgCourse) Nothing
    where
      courses = do
        courseEnts <- runDB $ selectList [] [Asc CourseName]
        optionsPairs $ Import.map (\ch -> (courseName $ entityVal ch, entityKey ch)) courseEnts
