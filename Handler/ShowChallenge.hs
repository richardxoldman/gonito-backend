module Handler.ShowChallenge where

import Import

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  defaultLayout $ do
    setTitle "Show a challenge"
    $(widgetFile "show-challenge")
