{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared
import Handler.SubmissionView
import Handler.TagUtils

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

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

data TableEntry = TableEntry {
  tableEntrySubmission :: Entity Submission,
  tableEntryVariant :: Entity Variant,
  tableEntrySubmitter :: Entity User,
  tableEntryMapping :: Map (Key Test) Evaluation,
  tableEntryTagsInfo :: [(Entity Tag, Entity SubmissionTag)],
  tableEntryParams :: [Entity Parameter],
  tableEntryRank :: Int }

tableEntryStamp :: TableEntry -> UTCTime
tableEntryStamp = submissionStamp . entityVal . tableEntrySubmission

submissionsTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App TableEntry
submissionsTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ Table.text "submitter" (formatSubmitter . entityVal . tableEntrySubmitter)
  ++ timestampCell "when" tableEntryStamp
  ++ descriptionCell mauthId
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScore k)) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\tableEntry -> (entityKey $ tableEntrySubmission tableEntry,
                                                                       entityVal $ tableEntrySubmission tableEntry,
                                                                       entityKey $ tableEntryVariant tableEntry,
                                                                       entityVal $ tableEntryVariant tableEntry,
                                                                       entityKey $ tableEntrySubmitter tableEntry,
                                                                       mauthId))

paramTable :: [Text] -> [Entity Test] -> Table App TableEntry
paramTable paramNames tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ mconcat (map paramExtractor paramNames)
  ++ mconcat (map (\(Entity k t) -> resultCell t (extractScore k)) tests)

paramExtractor :: Text -> Table App TableEntry
paramExtractor paramName = Table.text paramName (\entry ->
                                                   fromMaybe ""
                                                   $ listToMaybe
                                                   $ map parameterValue
                                                   $ filter (\p -> parameterName p == paramName)
                                                   $ map entityVal
                                                   $ tableEntryParams entry)

descriptionCell :: Maybe UserId -> Table App TableEntry
descriptionCell mauthId = Table.widget "description" (
  \(TableEntry (Entity _ s) (Entity _ v) (Entity u _) _ tagEnts paramEnts _) -> fragmentWithSubmissionTags
                                                                               (descriptionToBeShown s v (map entityVal paramEnts))
                                                                               (getInfoLink s u mauthId)
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
extractScore k tableEntry = lookup k $ tableEntryMapping tableEntry

leaderboardTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
leaderboardTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ leaderboardDescriptionCell mauthId
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

leaderboardDescriptionCell :: Maybe UserId -> Table App (a, LeaderboardEntry)
leaderboardDescriptionCell mauthId = Table.widget "description" (
  \(_,entry) -> fragmentWithSubmissionTags (descriptionToBeShown (leaderboardBestSubmission entry)
                                                                (leaderboardBestVariant entry)
                                                                (leaderboardParams entry))
                                          (getInfoLink (leaderboardBestSubmission entry)
                                                       (leaderboardUserId entry)
                                                       mauthId)
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

statusCellWidget :: Text -> RepoScheme -> Repo -> (SubmissionId, Submission, VariantId, Variant, UserId, Maybe UserId) -> WidgetFor App ()
statusCellWidget challengeName repoScheme challengeRepo (submissionId, submission, variantId, _, userId, mauthId) = $(widgetFile "submission-status")
    where commitHash = fromSHA1ToText $ submissionCommit submission
          isPublic = submissionIsPublic submission
          isOwner = (mauthId == Just userId)
          isVisible = checkWhetherVisible submission userId mauthId
          publicSubmissionBranch = getPublicSubmissionBranch submissionId
          maybeBrowsableUrl = if isPublic
                                then
                                  Just $ browsableGitRepoBranch repoScheme challengeRepo challengeName publicSubmissionBranch
                                else
                                  Nothing

getInfoLink :: Submission -> UserId -> Maybe UserId -> Maybe (Route App)
getInfoLink submission userId mauthId = if checkWhetherVisible submission userId mauthId
                                        then Just $ QueryResultsR commitHash
                                        else Nothing
   where commitHash = fromSHA1ToText $ submissionCommit submission

checkWhetherVisible :: Submission -> UserId -> Maybe UserId -> Bool
checkWhetherVisible submission userId mauthId = isPublic || isOwner
  where isPublic = submissionIsPublic submission
        isOwner = (mauthId == Just userId)

getAuxSubmissionEnts :: Key Test -> [TableEntry] -> [(Key User, (User, [(Entity Submission, Entity Variant, Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap (TableEntry s v (Entity ui u) m _ _ _) = (ui, (u, case Map.lookup testId m of
                                                                                       Just e -> [(s, v, e)]
                                                                                       Nothing -> []))


getLeaderboardEntriesByCriterion :: (Ord a) => Key Challenge
                                             -> ((Entity Submission) -> Bool)
                                             -> (TableEntry -> [a])
                                             -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntriesByCriterion challengeId condition selector = do
  (evaluationMaps, tests) <- runDB $ getChallengeSubmissionInfos condition challengeId
  let mainTests = getMainTests tests
  let mainTestEnt = getMainTest tests
  let (Entity mainTestId mainTest) = mainTestEnt
  let auxItems = concat
                 $ map (\i -> map (\s -> (s, [i])) (selector i))
                 $ filter (\entry -> member mainTestId $ tableEntryMapping entry)
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
  let (TableEntry bestSubmission bestVariant user evals _ _ _) = bestOne
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
           submissionComparator (TableEntry _  _  _ em1 _ _ _) (TableEntry _  _ _ em2 _ _ _) =
             (compareResult mainTest) (evaluationScore (em1 Map.! mainTestId))
                                      (evaluationScore (em2 Map.! mainTestId))

getLeaderboardEntries :: LeaderboardStyle -> Key Challenge -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntries BySubmitter challengeId =
  getLeaderboardEntriesByCriterion challengeId
                                   (const True)
                                   (\entry -> [entityKey $ tableEntrySubmitter entry])
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

getChallengeSubmissionInfos condition challengeId = do
  tests <- selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  let mainTest = getMainTest tests

  allSubmissionsVariants <- E.select $ E.from $ \(submission, variant) -> do
     E.where_ (submission ^. SubmissionChallenge E.==. E.val challengeId
               E.&&. submission ^. SubmissionIsHidden E.==. E.val False
               E.&&. variant ^. VariantSubmission E.==. submission ^. SubmissionId)
     return (submission, variant)

  scores <- mapM (getScore (entityKey mainTest)) $ map (entityKey . snd) allSubmissionsVariants

  let allSubmissionsVariantsWithRanks =
        sortBy (\(r1, (s1, _)) (r2, (s2, _)) -> (submissionStamp (entityVal s2) `compare` submissionStamp (entityVal s1))
                                                                               `thenCmp`
                                                                            (r2 `compare` r1))
        $ filter (\(_, (s, _)) -> condition s)
        $ map (\(rank, (_, sv)) -> (rank, sv))
        $ zip [1..]
        $ sortBy (\(s1, _) (s2, _) -> compareResult (entityVal mainTest) s2 s1)
        $ zip scores allSubmissionsVariants

  evaluationMaps <- mapM getEvaluationMap allSubmissionsVariantsWithRanks
  return (evaluationMaps, tests)

getScore testId variantId = do
  evaluations <- E.select $ E.from $ \(out, evaluation) -> do
                  E.where_ (out ^. OutVariant E.==. E.val variantId
                            E.&&. out ^. OutTest E.==. E.val testId
                            E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum
                            E.&&. evaluation ^. EvaluationTest E.==. E.val testId)
                  return evaluation
  return $ case evaluations of
             (e:_) -> evaluationScore $ entityVal e
             [] -> Nothing


getEvaluationMap (rank, (s@(Entity submissionId submission), v@(Entity variantId _))) = do
  outs <- selectList [OutVariant ==. variantId] []
  user <- get404 $ submissionSubmitter submission
  maybeEvaluations <- mapM (\(Entity _ o) -> getBy $ UniqueEvaluationTestChecksum (outTest o) (outChecksum o)) outs
  let evaluations = catMaybes maybeEvaluations
  let m = Map.fromList $ map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  tagEnts <- getTags submissionId

  parameters <- selectList [ParameterVariant ==. variantId] [Asc ParameterName]

  return $ TableEntry s v (Entity (submissionSubmitter submission) user) m tagEnts parameters rank
