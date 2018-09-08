module Handler.Presentation where

import Import

import Handler.ShowChallenge
import Handler.Tables

import qualified Yesod.Table as Table

import Text.Hamlet (hamletFile)

sampleChallengeName :: Text
sampleChallengeName = "petite-difference-challenge"

sampleChallengeName' :: Text
sampleChallengeName' = "retroc"

retrocChallengeName :: Text
retrocChallengeName = "retroc"

retroc2ChallengeName :: Text
retroc2ChallengeName = "retroc2"


sampleUserIdent :: Text
sampleUserIdent = "ptlen@ceti.pl"

getPresentation4RealR :: Handler Html
getPresentation4RealR = do
  readme <- challengeReadme sampleChallengeName

  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName sampleChallengeName

  (Just (Entity sampleUserId _)) <- runDB $ getBy $ UniqueUser sampleUserIdent
  let condition = (\(Entity _ submission) -> (submissionSubmitter submission == sampleUserId))
  (evaluationMaps', tests) <- getChallengeSubmissionInfos condition challengeId
  let evaluationMaps = take 10 evaluationMaps'

  sampleLeaderboard <- getSampleLeaderboard sampleChallengeName
  sampleLeaderboard' <- getSampleLeaderboard sampleChallengeName'

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  presentationLayout $(widgetFile "presentation-4real")

getPresentationDATeCH2017R = do
  readme <- challengeReadme retrocChallengeName
  retrocLeaderboard <- getSampleLeaderboard retrocChallengeName
  retroc2Leaderboard <- getSampleLeaderboard retroc2ChallengeName
  presentationLayout $(widgetFile "presentation-datech-2017")


getSampleLeaderboard :: Text -> HandlerFor App (WidgetFor App ())
getSampleLeaderboard name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name

  (leaderboard, (_, tests)) <- getLeaderboardEntries BySubmitter challengeId
  let leaderboardWithRanks = zip [1..] (take 10 leaderboard)

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  return $ Table.buildBootstrap (leaderboardTable Nothing
                                                  (challengeName challenge)
                                                  scheme challengeRepo tests)
                                leaderboardWithRanks

presentationLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(hamletFile "templates/presentation-layout.hamlet")
