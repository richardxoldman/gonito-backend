module Handler.ListChallenges where

import Import

mainCondition :: [Filter Challenge]
mainCondition = [ChallengeArchived !=. Just True]

getListChallengesR :: Handler Html
getListChallengesR = generalListChallenges mainCondition

getListChallengesJsonR :: Handler Value
getListChallengesJsonR = generalListChallengesJson mainCondition

getListArchivedChallengesR :: Handler Html
getListArchivedChallengesR = generalListChallenges [ChallengeArchived ==. Just True]

instance ToJSON (Entity Challenge) where
    toJSON (Entity _ ch) = object
        [ "link"  .= ("/challenge/" <> (challengeName ch))
        , "title" .= challengeTitle ch
        , "description" .= challengeDescription ch
        ]

generalListChallengesJson :: [Filter Challenge] -> Handler Value
generalListChallengesJson filterExpr = do
  challenges <- getChallenges filterExpr
  return $ toJSON challenges

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
