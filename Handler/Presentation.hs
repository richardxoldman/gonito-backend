module Handler.Presentation where

import Import

import Handler.ShowChallenge
import Handler.Tables

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import Text.Hamlet (hamletFile)

sampleChallengeName :: Text
sampleChallengeName = "petite-difference-challenge"

sampleChallengeName' :: Text
sampleChallengeName' = "retroc"


sampleUserIdent :: Text
sampleUserIdent = "ptlen@ceti.pl"

getPresentation4RealR :: Handler Html
getPresentation4RealR = do
  readme <- challengeReadme sampleChallengeName

  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName sampleChallengeName

  (Just (Entity sampleUserId _)) <- runDB $ getBy $ UniqueUser sampleUserIdent
  let condition = (\(Entity _ submission) -> (submissionSubmitter submission == sampleUserId))
  (evaluationMaps', tests) <- getChallengeSubmissionInfos condition challengeId
  let evaluationMaps = take 10 evaluationMaps'

  sampleLeaderboard <- getSampleLeaderboard sampleChallengeName
  sampleLeaderboard' <- getSampleLeaderboard sampleChallengeName'

  presentationLayout $(widgetFile "presentation-4real")

getPresentationDATeCH2017R = do
  readme <- challengeReadme sampleChallengeName
  sampleLeaderboard <- getSampleLeaderboard sampleChallengeName'
  presentationLayout $(widgetFile "presentation-datech-2017")


getSampleLeaderboard name = do
  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name

  Just repo <- runDB $ get $ challengePublicRepo challenge
  (test, leaderboard) <- getLeaderboardEntries challengeId
  let leaderboardWithRanks = zip [1..] (take 10 leaderboard)

  return $ Table.buildBootstrap (leaderboardTable Nothing (challengeName challenge) test) leaderboardWithRanks

presentationLayout widget = do
  master <- getYesod
  mmsg <- getMessage

  maybeUser <- maybeAuth

  pc <- widgetToPageContent widget
  withUrlRenderer $(hamletFile "templates/presentation-layout.hamlet")
