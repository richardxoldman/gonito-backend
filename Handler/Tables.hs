{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Maybe as DM

import qualified Data.List as DL

import GEval.Core

data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardBestSubmission :: Submission,
  leaderboardEvaluation :: Evaluation,
  leaderboardNumberOfSubmissions :: Int
}

submissionsTable :: [Entity Test] -> Table site (Entity Submission, Entity User, Map (Key Test) Evaluation)
submissionsTable tests = mempty
  ++ Table.text "submitter" (formatSubmitter . \(_, Entity _ submitter, _) -> submitter)
  ++ Table.string "when" (show . submissionStamp . \(Entity _ s, _, _) -> s)
  ++ Table.text "description" (submissionDescription . \(Entity _ s, _,  _) -> s)
  ++ mconcat (map (\(Entity k t) -> Table.string (testName t) (submissionScore k)) tests)


leaderboardTable :: Table site (Int, LeaderboardEntry)
leaderboardTable = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ Table.string "when" (show . submissionStamp . leaderboardBestSubmission . snd)
  ++ Table.text "description" (submissionDescription . leaderboardBestSubmission . snd)
  ++ Table.string "result" (presentScore . leaderboardEvaluation . snd)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)


getMainTest :: [Entity Test] -> Entity Test
getMainTest tests = DL.maximumBy (\(Entity _ a) (Entity _ b) -> ((testName a) `compare` (testName b))) tests

getAuxSubmissions :: Key Test -> [(Entity Submission, Entity User, Map (Key Test) Evaluation)] -> [(Key User, (User, [(Submission, Evaluation)]))]
getAuxSubmissions testId evaluationMaps = map (processEvaluationMap testId) evaluationMaps
   where processEvaluationMap testId ((Entity _ s), (Entity ui u), m) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, e)]
                                                                                       Nothing -> []))

getLeaderboardEntries :: Key Challenge -> Handler [LeaderboardEntry]
getLeaderboardEntries challengeId = do
  (evaluationMaps, tests) <- getChallengeSubmissionInfos (\_ -> True) challengeId
  let mainTestEnt = getMainTest tests
  let (Entity mainTestId mainTest) = mainTestEnt
  let auxSubmissions = getAuxSubmissions mainTestId evaluationMaps
  let submissionsByUser = Map.fromListWith (\(u1, l1) (_, l2) -> (u1, l1++l2)) auxSubmissions
  let entryComparator a b = (compareResult mainTest) (evaluationScore $ leaderboardEvaluation a) (evaluationScore $ leaderboardEvaluation b)
  let entries = sortBy (flip entryComparator) $ map (toEntry mainTest) $ filter (\(_, (_, s)) -> not (null s)) $ Map.toList submissionsByUser
  return entries
    where submissionComparator mainTest (_, e1) (_, e2) = (compareResult mainTest) (evaluationScore e1) (evaluationScore e2)
          toEntry mainTest (_, (u, ss)) = LeaderboardEntry {
              leaderboardUser = u,
              leaderboardBestSubmission = fst bestOne,
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


formatSubmitter :: User -> Text
formatSubmitter user = case userName user of
  Just name -> name
  Nothing -> "[name not given]"

submissionScore :: Key Test -> (Entity Submission, Entity User, Map (Key Test) Evaluation) -> String
submissionScore k (_, _, m) = fromMaybe "N/A" (presentScore <$> lookup k m)

presentScore :: Evaluation -> String
presentScore evaluation = fromMaybe "???" (show <$> evaluationScore evaluation)
