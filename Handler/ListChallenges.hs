module Handler.ListChallenges where

import Import

getListChallengesR :: Handler Html
getListChallengesR = do
  challenges <- runDB $ selectList [] [Desc ChallengeStarred, Desc ChallengeStamp]
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

listChallengesCore challenges = $(widgetFile "list-challenges-core")

getChallengeImageR :: ChallengeId -> Handler Html
getChallengeImageR challengeId = do
   challenge <- runDB $ get404 challengeId
   case challengeImage challenge of
     Just image -> do
       addHeader "Content-Disposition" "attachment; filename=\"image.png\""
       sendResponse (typePng, toContent image)
     Nothing -> sendResponseStatus status202 ()
