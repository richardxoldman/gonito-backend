{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe as DM

import qualified Data.List as DL

import Data.Text (pack)

import PersistSHA1

import GEval.Core

import Text.Printf

data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardUserId :: UserId,
  leaderboardBestSubmission :: Submission,
  leaderboardBestSubmissionId :: SubmissionId,
  leaderboardEvaluation :: Evaluation,
  leaderboardNumberOfSubmissions :: Int
}

submissionsTable :: Maybe UserId -> Text -> [Entity Test] -> Table App (Entity Submission, Entity User, Map (Key Test) Evaluation)
submissionsTable mauthId challengeName tests = mempty
  ++ Table.text "submitter" (formatSubmitter . (\(_, Entity _ submitter, _) -> submitter))
  ++ timestampCell "when" (submissionStamp . (\(Entity _ s, _, _) -> s))
  ++ Table.text "description" (submissionDescription . (\(Entity _ s, _,  _) -> s))
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScore k)) tests)
  ++ statusCell challengeName (\(Entity submissionId submission, Entity userId _, _) -> (submissionId, submission, userId, mauthId))

extractScore :: Key Test -> (Entity Submission, Entity User, Map (Key Test) Evaluation) -> Maybe Evaluation
extractScore k (_, _, m) = lookup k m

leaderboardTable :: Maybe UserId -> Text -> Test -> Table App (Int, LeaderboardEntry)
leaderboardTable mauthId challengeName test = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ Table.text "description" (submissionDescription . leaderboardBestSubmission . snd)
  ++ resultCell test ((\e -> Just e) . leaderboardEvaluation . snd)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)
  ++ statusCell challengeName (\(_, e) -> (leaderboardBestSubmissionId e,
                                       leaderboardBestSubmission e,
                                       leaderboardUserId e,
                                       mauthId))


hoverTextCell :: Text -> (a -> Text) -> (a -> Text) -> Table site a
hoverTextCell h mainTextFun hoverTextFun = Table.widget h (
  \v -> [whamlet|<span title="#{hoverTextFun v}">#{mainTextFun v}|])

timestampCell :: Text -> (a -> UTCTime) -> Table site a
timestampCell h timestampFun = hoverTextCell h (Data.Text.pack . shorterFormat . timestampFun) (Data.Text.pack . show . timestampFun)
   where shorterFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

statusCell :: Text -> (a -> (SubmissionId, Submission, UserId, Maybe UserId)) -> Table App a
statusCell challengeName fun = Table.widget "" (statusCellWidget challengeName . fun)

resultCell :: Test -> (a -> Maybe Evaluation) -> Table App a
resultCell test fun = hoverTextCell ((testName test) ++ "/" ++ (Data.Text.pack $ show $ testMetric test)) (formatTruncatedScore (testPrecision test) . fun) (formatFullScore . fun)

formatFullScore :: Maybe Evaluation -> Text
formatFullScore (Just evaluation) = fromMaybe "???" (Data.Text.pack <$> show <$> evaluationScore evaluation)
formatFullScore Nothing = "N/A"

formatTruncatedScore :: Maybe Int -> Maybe Evaluation -> Text
formatTruncatedScore Nothing e = formatFullScore e
formatTruncatedScore _ Nothing  = formatFullScore Nothing
formatTruncatedScore (Just precision) (Just evaluation) = case evaluationScore evaluation of
  Just score -> Data.Text.pack $ printf "%0.*f" precision score
  Nothing -> formatFullScore Nothing


statusCellWidget challengeName (submissionId, submission, userId, mauthId) = $(widgetFile "submission-status")
    where commitHash = fromSHA1ToText $ submissionCommit submission
          isPublic = submissionIsPublic submission
          isOwner = (mauthId == Just userId)
          isVisible = isPublic || isOwner
          publicSubmissionBranch = getPublicSubmissionBranch submissionId
          maybeBrowsableUrl = if isPublic
                                then
                                  Just $ browsableGitRepoBranch challengeName publicSubmissionBranch
                                else
                                  Nothing

getMainTest :: [Entity Test] -> Entity Test
getMainTest tests = DL.maximumBy (\(Entity _ a) (Entity _ b) -> ((testName a) `compare` (testName b))) tests

getAuxSubmissions :: Key Test -> [(Entity Submission, Entity User, Map (Key Test) Evaluation)] -> [(Key User, (User, [(Submission, Evaluation)]))]
getAuxSubmissions testId evaluationMaps = map (processEvaluationMap testId) evaluationMaps
   where processEvaluationMap testId ((Entity _ s), (Entity ui u), m) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, e)]
                                                                                       Nothing -> []))


getAuxSubmissionEnts :: Key Test -> [(Entity Submission, Entity User, Map (Key Test) Evaluation)] -> [(Key User, (User, [((Entity Submission), Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map (processEvaluationMap testId) evaluationMaps
   where processEvaluationMap testId (s, (Entity ui u), m) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, e)]
                                                                                       Nothing -> []))




getLeaderboardEntries :: Key Challenge -> Handler (Test, [LeaderboardEntry])
getLeaderboardEntries challengeId = do
  (evaluationMaps, tests) <- getChallengeSubmissionInfos (\_ -> True) challengeId
  let mainTestEnt = getMainTest tests
  let (Entity mainTestId mainTest) = mainTestEnt
  let auxSubmissions = getAuxSubmissionEnts mainTestId evaluationMaps
  let submissionsByUser = Map.fromListWith (\(u1, l1) (_, l2) -> (u1, l1++l2)) auxSubmissions
  let entryComparator a b = (compareResult mainTest) (evaluationScore $ leaderboardEvaluation a) (evaluationScore $ leaderboardEvaluation b)
  let entries = sortBy (flip entryComparator) $ map (toEntry mainTest) $ filter (\(_, (_, s)) -> not (null s)) $ Map.toList submissionsByUser
  return (mainTest, entries)
    where submissionComparator mainTest (_, e1) (_, e2) = (compareResult mainTest) (evaluationScore e1) (evaluationScore e2)
          toEntry mainTest (ui, (u, ss)) = LeaderboardEntry {
              leaderboardUser = u,
              leaderboardUserId = ui,
              leaderboardBestSubmission = (\(Entity _ s) -> s) $ fst bestOne,
              leaderboardBestSubmissionId = (\(Entity si _) -> si) $ fst bestOne,
              leaderboardEvaluation = snd bestOne,
              leaderboardNumberOfSubmissions = length ss }
                  where bestOne = DL.maximumBy (submissionComparator mainTest) ss

compareResult :: Test -> Maybe Double -> Maybe Double -> Ordering
compareResult test (Just x) (Just y) = (compareFun $ getMetricOrdering $ testMetric test) x y
compareResult _ (Just _) Nothing = GT
compareResult _ Nothing (Just _) = LT
compareResult _ Nothing Nothing = EQ

compareFun :: MetricOrdering -> Double -> Double -> Ordering
compareFun TheLowerTheBetter = flip compare
compareFun TheHigherTheBetter = compare

getChallengeSubmissionInfos :: ((Entity Submission) -> Bool) -> Key Challenge -> Handler ([(Entity Submission, Entity User, Map (Key Test) Evaluation)], [Entity Test])
getChallengeSubmissionInfos condition challengeId = do
  allSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId] [Desc SubmissionStamp]
  let submissions = filter condition allSubmissions
  tests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  evaluationMaps <- mapM getEvaluationMap submissions
  return (evaluationMaps, tests)

getEvaluationMap :: Entity Submission -> Handler (Entity Submission, Entity User, Map (Key Test) Evaluation)
getEvaluationMap s@(Entity submissionId submission) = do
  outs <- runDB $ selectList [OutSubmission ==. submissionId] []
  user <- runDB $ get404 $ submissionSubmitter submission
  maybeEvaluations <- runDB $ mapM (\(Entity _ o) -> getBy $ UniqueEvaluationTestChecksum (outTest o) (outChecksum o)) outs
  let evaluations = catMaybes maybeEvaluations
  let m = Map.fromList $ map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  return (s, Entity (submissionSubmitter submission) user, m)
