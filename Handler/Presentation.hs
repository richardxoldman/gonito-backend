module Handler.Presentation where

import Import

import Handler.ShowChallenge
import Handler.Tables

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import Text.Hamlet (hamletFile)

sampleChallengeName :: Text
sampleChallengeName = "petite-difference-challenge"

getPresentation4RealR :: Handler Html
getPresentation4RealR = do
  readme <- challengeReadme sampleChallengeName

  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName sampleChallengeName
  Just repo <- runDB $ get $ challengePublicRepo challenge
  (test, leaderboard) <- getLeaderboardEntries challengeId
  let leaderboardWithRanks = zip [1..] leaderboard

  presentationLayout $(widgetFile "presentation-4real")

presentationLayout widget = do
  master <- getYesod
  mmsg <- getMessage

  maybeUser <- maybeAuth

  pc <- widgetToPageContent widget
  withUrlRenderer $(hamletFile "templates/presentation-layout.hamlet")
