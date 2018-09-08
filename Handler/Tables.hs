{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared
import Handler.SubmissionView
import Handler.TagUtils

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Data.Map as Map

import Data.Text (pack, unpack, unwords)

import PersistSHA1

import qualified Data.List as DL

import GEval.Core

import GEval.ParseParams (parseParamsFromFilePath, OutputFileParsed(..))

data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardUserId :: UserId,
  leaderboardBestSubmission :: Submission,
  leaderboardBestSubmissionId :: SubmissionId,
  leaderboardBestVariant :: Variant,
  leaderboardBestVariantId :: VariantId,
  leaderboardEvaluationMap :: Map (Key Test) Evaluation,
  leaderboardNumberOfSubmissions :: Int,
  leaderboardTags :: [(Entity Tag, Entity SubmissionTag)],
  leaderboardParams :: [Parameter]
}

data TableEntry = TableEntry (Entity Submission)
                             (Entity Variant)
                             (Entity User)
                             (Map (Key Test) Evaluation)
                             [(Entity Tag, Entity SubmissionTag)]
                             [Entity Parameter]

-- TODO change into a record
tableEntryParams (TableEntry _ _ _ _ _ paramEnts) = paramEnts
tableEntryMapping (TableEntry _ _ _ mapping _ _) = mapping
tableEntryTagsInfo (TableEntry _ _ _ _ tagsInfo _) = tagsInfo

submissionsTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App TableEntry
submissionsTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.text "submitter" (formatSubmitter . (\(TableEntry _  _ (Entity _ submitter) _ _ _) -> submitter))
  ++ timestampCell "when" (submissionStamp . (\(TableEntry (Entity _ s) _ _ _ _ _) -> s))
  ++ descriptionCell
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScore k)) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\(TableEntry (Entity submissionId submission) (Entity variantId variant) (Entity userId _) _ _ _) -> (submissionId, submission, variantId, variant, userId, mauthId))

descriptionCell :: Table site TableEntry
descriptionCell = Table.widget "description" (
  \(TableEntry (Entity _ s) (Entity _ v) _ _ tagEnts paramEnts) -> fragmentWithSubmissionTags
                                                                      (descriptionToBeShown s v (map entityVal paramEnts))
                                                                      tagEnts)


descriptionToBeShown :: Submission -> Variant -> [Parameter] -> Text
descriptionToBeShown s v params = (submissionDescription s) ++ (Data.Text.pack vdescription) ++ " " ++ paramsShown
  where (OutputFileParsed r _) = parseParamsFromFilePath (Data.Text.unpack $ variantName v)
        vdescription = if r == "out"
                         then
                           ""
                         else
                           " " ++ r
        paramsShown = Data.Text.unwords $ map formatParameter params

extractScore :: Key Test -> TableEntry -> Maybe Evaluation
extractScore k (TableEntry _  _  _ m _ _) = lookup k m

leaderboardTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
leaderboardTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ leaderboardDescriptionCell
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScoreFromLeaderboardEntry k . snd)) tests)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                       leaderboardBestSubmission e,
                                       leaderboardBestVariantId e,
                                       leaderboardBestVariant e,
                                       leaderboardUserId e,
                                       mauthId))

extractScoreFromLeaderboardEntry :: Key Test -> LeaderboardEntry -> Maybe Evaluation
extractScoreFromLeaderboardEntry k entry = lookup k (leaderboardEvaluationMap entry)

leaderboardDescriptionCell :: Table site (a, LeaderboardEntry)
leaderboardDescriptionCell = Table.widget "description" (
  \(_,entry) -> fragmentWithSubmissionTags (descriptionToBeShown (leaderboardBestSubmission entry)
                                                                (leaderboardBestVariant entry)
                                                                (leaderboardParams entry))
                                          (leaderboardTags entry)
  )



hoverTextCell :: Text -> (a -> Text) -> (a -> Text) -> Table site a
hoverTextCell h mainTextFun hoverTextFun = Table.widget h (
  \v -> [whamlet|<span title="#{hoverTextFun v}">#{mainTextFun v}|])

timestampCell :: Text -> (a -> UTCTime) -> Table site a
timestampCell h timestampFun = hoverTextCell h (Data.Text.pack . shorterFormat . timestampFun) (Data.Text.pack . show . timestampFun)
   where shorterFormat = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

statusCell :: Text -> RepoScheme -> Repo -> (a -> (SubmissionId, Submission, VariantId, Variant, UserId, Maybe UserId)) -> Table App a
statusCell challengeName repoScheme challengeRepo fun = Table.widget "" (statusCellWidget challengeName repoScheme challengeRepo . fun)

resultCell :: Test -> (a -> Maybe Evaluation) -> Table App a
resultCell test fun = hoverTextCell (formatTestForHtml test) (formatTruncatedScore (testPrecision test) . fun) (formatFullScore . fun)

statusCellWidget :: Eq a => Text -> RepoScheme -> Repo -> (SubmissionId, Submission, VariantId, Variant, a, Maybe a) -> WidgetFor App ()
statusCellWidget challengeName repoScheme challengeRepo (submissionId, submission, variantId, _, userId, mauthId) = $(widgetFile "submission-status")
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

getAuxSubmissionEnts :: Key Test -> [TableEntry] -> [(Key User, (User, [(Entity Submission, Entity Variant, Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap (TableEntry s v (Entity ui u) m _ _) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, v, e)]
                                                                                       Nothing -> []))


getLeaderboardEntriesByCriterion :: (Ord a) => Key Challenge
                                             -> ((Entity Submission) -> Bool)
                                             -> (TableEntry -> [a])
                                             -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntriesByCriterion challengeId condition selector = do
  (evaluationMaps, tests) <- getChallengeSubmissionInfos condition challengeId
  let mainTests = getMainTests tests
  let mainTestEnt = getMainTest tests
  let (Entity mainTestId mainTest) = mainTestEnt
  let auxItems = concat
                 $ map (\i -> map (\s -> (s, [i])) (selector i))
                 $ filter (\(TableEntry _ _ _ em _ _) -> member mainTestId em)
                 $ evaluationMaps
  let auxItemsMap = Map.fromListWith (++) auxItems
  let entryComparator a b = (compareResult mainTest) (evaluationScore $ leaderboardEvaluationMap a Map.! mainTestId)
                                                     (evaluationScore $ leaderboardEvaluationMap b Map.! mainTestId)
  entries' <- mapM (toLeaderboardEntry challengeId mainTests)
             $ filter (\ll -> not (null ll))
             $ map snd
             $ Map.toList auxItemsMap
  let entries = DL.nubBy (\a b -> leaderboardBestVariantId a == leaderboardBestVariantId b)
                $ sortBy (flip entryComparator) entries'
  return (entries, (evaluationMaps, mainTests))

toLeaderboardEntry :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistQueryRead (YesodPersistBackend site), YesodPersist site, Foldable t) => Key Challenge -> [Entity Test] -> t TableEntry -> HandlerFor site LeaderboardEntry
toLeaderboardEntry challengeId tests ss = do
  let bestOne = DL.maximumBy submissionComparator ss
  let (TableEntry bestSubmission bestVariant user evals _ _) = bestOne
  let submissionId = entityKey bestSubmission
  tagEnts <- runDB $ getTags submissionId

  parameters <- runDB $ selectList [ParameterVariant ==. (entityKey bestVariant)] [Asc ParameterName]

  -- get all user submissions, including hidden ones
  allUserSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId,
                                           SubmissionSubmitter ==. entityKey user]
                                          [Desc SubmissionStamp]
  return $ LeaderboardEntry {
              leaderboardUser = entityVal user,
              leaderboardUserId = entityKey user,
              leaderboardBestSubmission = entityVal bestSubmission,
              leaderboardBestSubmissionId = entityKey bestSubmission,
              leaderboardBestVariant = entityVal bestVariant,
              leaderboardBestVariantId = entityKey bestVariant,
              leaderboardEvaluationMap = evals,
              leaderboardNumberOfSubmissions = length allUserSubmissions,
              leaderboardTags = tagEnts,
              leaderboardParams = map entityVal parameters
              }
     where (Entity mainTestId mainTest) = getMainTest tests
           submissionComparator (TableEntry _  _  _ em1 _ _) (TableEntry _  _ _ em2 _ _) =
             (compareResult mainTest) (evaluationScore (em1 Map.! mainTestId))
                                      (evaluationScore (em2 Map.! mainTestId))

getLeaderboardEntries :: LeaderboardStyle -> Key Challenge -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntries BySubmitter challengeId =
  getLeaderboardEntriesByCriterion challengeId
                                   (const True)
                                   (\(TableEntry _ _ (Entity userId _) _ _ _) -> [userId])
getLeaderboardEntries ByTag challengeId =
  getLeaderboardEntriesByCriterion challengeId
                                   (const True)
                                   (noEmptyList . (map (entityKey . fst)) . tableEntryTagsInfo)
  where noEmptyList [] = [Nothing]
        noEmptyList l = map Just l

compareResult :: Test -> Maybe Double -> Maybe Double -> Ordering
compareResult test (Just x) (Just y) = (compareFun $ getMetricOrdering $ testMetric test) x y
compareResult _ (Just _) Nothing = GT
compareResult _ Nothing (Just _) = LT
compareResult _ Nothing Nothing = EQ

compareFun :: MetricOrdering -> Double -> Double -> Ordering
compareFun TheLowerTheBetter = flip compare
compareFun TheHigherTheBetter = compare

getChallengeSubmissionInfos :: ((Entity Submission) -> Bool)
                              -> Key Challenge
                              -> Handler ([TableEntry], [Entity Test])
getChallengeSubmissionInfos condition challengeId = do
  allSubmissions <- runDB $ selectList [SubmissionChallenge ==. challengeId,
                                       SubmissionIsHidden !=. Just True]
                                      [Desc SubmissionStamp]
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

  parameters <- runDB $ selectList [ParameterVariant ==. variantId] [Asc ParameterName]

  return $ TableEntry s v (Entity (submissionSubmitter submission) user) m tagEnts parameters
