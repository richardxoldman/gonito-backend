module Handler.ListChallenges where

import Import

getListChallengesR :: Handler Html
getListChallengesR = generalListChallenges [ChallengeArchived !=. Just True]

getListArchivedChallengesR :: Handler Html
getListArchivedChallengesR = generalListChallenges [ChallengeArchived ==. Just True]

generalListChallenges :: [Filter Challenge] -> Handler Html
generalListChallenges filterExpr = do
  challenges <- getChallenges filterExpr
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

getChallenges :: [Filter Challenge] -> Handler [Entity Challenge]
getChallenges filterExpr = runDB $ selectList filterExpr [Desc ChallengeStarred, Desc ChallengeStamp]

listChallengesCore :: [Entity Challenge] -> Widget
listChallengesCore challenges = $(widgetFile "list-challenges-core")

getChallengeImageR :: ChallengeId -> Handler Html
getChallengeImageR challengeId = do
   challenge <- runDB $ get404 challengeId
   case challengeImage challenge of
     Just image -> do
       addHeader "Content-Disposition" "attachment; filename=\"image.png\""
       sendResponse (typePng, toContent image)
     Nothing -> sendResponseStatus status202 ()
