{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared
import Handler.SubmissionView
import Handler.TagUtils

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Data.Map as Map

import Data.Text (pack)

import PersistSHA1

import qualified Data.List as DL

import GEval.Core


data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardUserId :: UserId,
  leaderboardBestSubmission :: Submission,
  leaderboardBestSubmissionId :: SubmissionId,
  leaderboardEvaluation :: Evaluation,
  leaderboardNumberOfSubmissions :: Int,
  leaderboardTags :: [(Entity Tag, Entity SubmissionTag)]
}

data TableEntry = TableEntry (Entity Submission)
                             (Entity Variant)
                             (Entity User)
                             (Map (Key Test) Evaluation)
                             [(Entity Tag, Entity SubmissionTag)]

submissionsTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App TableEntry
submissionsTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.text "submitter" (formatSubmitter . (\(TableEntry _  _ (Entity _ submitter) _ _) -> submitter))
  ++ timestampCell "when" (submissionStamp . (\(TableEntry (Entity _ s) _ _ _ _) -> s))
  ++ descriptionCell
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScore k)) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\(TableEntry (Entity submissionId submission) _ (Entity userId _) _ _) -> (submissionId, submission, userId, mauthId))

descriptionCell :: Table site TableEntry
descriptionCell = Table.widget "description" (
  \(TableEntry (Entity _ s) _  _ _ tagEnts) -> fragmentWithSubmissionTags (submissionDescription s) tagEnts)

extractScore :: Key Test -> TableEntry -> Maybe Evaluation
extractScore k (TableEntry _  _  _ m _) = lookup k m

leaderboardTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> Test -> Table App (Int, LeaderboardEntry)
leaderboardTable mauthId challengeName repoScheme challengeRepo test = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ leaderboardDescriptionCell
  ++ resultCell test ((\e -> Just e) . leaderboardEvaluation . snd)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                       leaderboardBestSubmission e,
                                       leaderboardUserId e,
                                       mauthId))

leaderboardDescriptionCell :: Table site (a, LeaderboardEntry)
leaderboardDescriptionCell = Table.widget "description" (
  \(_,entry) -> fragmentWithSubmissionTags (submissionDescription $ leaderboardBestSubmission entry) (leaderboardTags entry))



hoverTextCell :: Text -> (a -> Text) -> (a -> Text) -> Table site a
hoverTextCell h mainTextFun hoverTextFun = Table.widget h (
  \v -> [whamlet|<span title="#{hoverTextFun v}">#{mainTextFun v}|])

timestampCell :: Text -> (a -> UTCTime) -> Table site a
timestampCell h timestampFun = hoverTextCell h (Data.Text.pack . shorterFormat . timestampFun) (Data.Text.pack . show . timestampFun)
   where shorterFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

statusCell :: Text -> RepoScheme -> Repo -> (a -> (SubmissionId, Submission, UserId, Maybe UserId)) -> Table App a
statusCell challengeName repoScheme challengeRepo fun = Table.widget "" (statusCellWidget challengeName repoScheme challengeRepo . fun)

resultCell :: Test -> (a -> Maybe Evaluation) -> Table App a
resultCell test fun = hoverTextCell ((testName test) ++ "/" ++ (Data.Text.pack $ show $ testMetric test)) (formatTruncatedScore (testPrecision test) . fun) (formatFullScore . fun)

statusCellWidget :: Eq a => Text -> RepoScheme -> Repo -> (SubmissionId, Submission, a, Maybe a) -> WidgetFor App ()
statusCellWidget challengeName repoScheme challengeRepo (submissionId, submission, userId, mauthId) = $(widgetFile "submission-status")
    where commitHash = fromSHA1ToText $ submissionCommit submission
          isPublic = submissionIsPublic submission
          isOwner = (mauthId == Just userId)
          isVisible = isPublic || isOwner
          publicSubmissionBranch = getPublicSubmissionBranch submissionId
          maybeBrowsableUrl = if isPublic
                                then
                                  Just $ browsableGitRepoBranch repoScheme challengeRepo challengeName publicSubmissionBranch
                                else
                                  Nothing

getAuxSubmissions :: Key Test -> [(Entity Submission, Entity User, Map (Key Test) Evaluation)] -> [(Key User, (User, [(Submission, Evaluation)]))]
getAuxSubmissions testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap ((Entity _ s), (Entity ui u), m) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, e)]
                                                                                       Nothing -> []))


getAuxSubmissionEnts :: Key Test -> [TableEntry] -> [(Key User, (User, [((Entity Submission), Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap (TableEntry s _ (Entity ui u) m _) = (ui, (u, case Map.lookup testId m of
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
  entries' <- mapM (toEntry challengeId mainTest) $ filter (\(_, (_, s)) -> not (null s)) $ Map.toList submissionsByUser
  let entries = sortBy (flip entryComparator) entries'
  return (mainTest, entries)


toEntry :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistQueryRead (YesodPersistBackend site), YesodPersist site, Foldable t) => Key Challenge -> Test -> (Key User, (User, t (Entity Submission, Evaluation))) -> HandlerFor site LeaderboardEntry
toEntry challengeId mainTest (ui, (u, ss)) = do
  let bestOne = DL.maximumBy submissionComparator ss
  let submissionId = entityKey $ fst bestOne
  tagEnts <- runDB $ getTags submissionId
  -- get all user submissions, including hidden ones
  allUserSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId, SubmissionSubmitter ==. ui] [Desc SubmissionStamp]
  return $ LeaderboardEntry {
              leaderboardUser = u,
              leaderboardUserId = ui,
              leaderboardBestSubmission = (\(Entity _ s) -> s) $ fst bestOne,
              leaderboardBestSubmissionId = (\(Entity si _) -> si) $ fst bestOne,
              leaderboardEvaluation = snd bestOne,
              leaderboardNumberOfSubmissions = length allUserSubmissions,
              leaderboardTags = tagEnts
              }
     where submissionComparator (_, e1) (_, e2) = (compareResult mainTest) (evaluationScore e1) (evaluationScore e2)


compareResult :: Test -> Maybe Double -> Maybe Double -> Ordering
compareResult test (Just x) (Just y) = (compareFun $ getMetricOrdering $ testMetric test) x y
compareResult _ (Just _) Nothing = GT
compareResult _ Nothing (Just _) = LT
compareResult _ Nothing Nothing = EQ

compareFun :: MetricOrdering -> Double -> Double -> Ordering
compareFun TheLowerTheBetter = flip compare
compareFun TheHigherTheBetter = compare

getChallengeSubmissionInfos :: ((Entity Submission) -> Bool) -> Key Challenge -> Handler ([TableEntry], [Entity Test])
getChallengeSubmissionInfos condition challengeId = do
  allSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId, SubmissionIsHidden !=. Just True] [Desc SubmissionStamp]
  let submissions = filter condition allSubmissions
  tests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  evaluationMaps <- mapM getEvaluationMapForSubmission submissions
  return (concat evaluationMaps, tests)

getEvaluationMapForSubmission :: Entity Submission -> Handler [TableEntry]
getEvaluationMapForSubmission s@(Entity submissionId _)= do
  variants <- runDB $ selectList [VariantSubmission ==. submissionId] []
  mapM (getEvaluationMap s) variants

getEvaluationMap :: Entity Submission -> Entity Variant -> Handler TableEntry
getEvaluationMap s@(Entity submissionId submission) v@(Entity variantId _) = do
  outs <- runDB $ selectList [OutVariant ==. variantId] []
  user <- runDB $ get404 $ submissionSubmitter submission
  maybeEvaluations <- runDB $ mapM (\(Entity _ o) -> getBy $ UniqueEvaluationTestChecksum (outTest o) (outChecksum o)) outs
  let evaluations = catMaybes maybeEvaluations
  let m = Map.fromList $ map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  tagEnts <- runDB $ getTags submissionId
  return $ TableEntry s v (Entity (submissionSubmitter submission) user) m tagEnts
