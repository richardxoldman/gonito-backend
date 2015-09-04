module Handler.ListChallenges where

import Import

getListChallengesR :: Handler Html
getListChallengesR = do
  challenges' <- runDB $ selectList [] [Desc ChallengeStamp]
  let challenges = map (\(Entity _ v) -> v) challenges'
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

listChallengesCore challenges = $(widgetFile "list-challenges-core")
