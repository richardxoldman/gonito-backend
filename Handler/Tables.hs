{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Tables where

import Import
import Handler.Shared
import Handler.Evaluate
import Handler.SubmissionView
import Handler.TagUtils

import qualified Yesod.Table as Table
import Yesod.Table (Table)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import qualified Data.Map as Map

import Data.Text (pack, unpack, unwords, take)

import PersistSHA1

import qualified Data.List as DL

import GEval.Core
import GEval.EvaluationScheme

import GEval.ParseParams (parseParamsFromFilePath, OutputFileParsed(..))

data TestReference = TestReference Text Text
                     deriving (Show, Eq, Ord)

getTestReference :: Entity Test -> TestReference
getTestReference (Entity _ test) = TestReference (Data.Text.pack $ show $ testMetric test) (testName test)

data LeaderboardEntry = LeaderboardEntry {
  leaderboardUser :: User,
  leaderboardUserId :: UserId,
  leaderboardBestSubmission :: Submission,
  leaderboardBestSubmissionId :: SubmissionId,
  leaderboardBestVariant :: Variant,
  leaderboardBestVariantId :: VariantId,
  leaderboardEvaluationMap :: Map TestReference Evaluation,
  leaderboardNumberOfSubmissions :: Int,
  leaderboardTags :: [(Entity Tag, Entity SubmissionTag)],
  leaderboardParams :: [Parameter],
  leaderboardVersion :: (Int, Int, Int)
}

data TableEntry = TableEntry {
  tableEntrySubmission :: Entity Submission,
  tableEntryVariant :: Entity Variant,
  tableEntrySubmitter :: Entity User,
  tableEntryMapping :: Map TestReference Evaluation,
  tableEntryTagsInfo :: [(Entity Tag, Entity SubmissionTag)],
  tableEntryParams :: [Entity Parameter],
  tableEntryRank :: Int,
  tableEntryVersion :: (Int, Int, Int) }

tableEntryStamp :: TableEntry -> UTCTime
tableEntryStamp = submissionStamp . entityVal . tableEntrySubmission

submissionsTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App TableEntry
submissionsTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ Table.text "submitter" (formatSubmitter . entityVal . tableEntrySubmitter)
  ++ timestampCell "when" tableEntryStamp
  ++ Table.text "ver." (formatVersion . tableEntryVersion)
  ++ descriptionCell mauthId
  ++ mconcat (map (\e@(Entity _ t) -> resultCell t (extractScore $ getTestReference e)) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\tableEntry -> (entityKey $ tableEntrySubmission tableEntry,
                                                                       entityVal $ tableEntrySubmission tableEntry,
                                                                       entityKey $ tableEntryVariant tableEntry,
                                                                       entityVal $ tableEntryVariant tableEntry,
                                                                       entityKey $ tableEntrySubmitter tableEntry,
                                                                       mauthId))

variantTable :: [Text] -> [Entity Test] -> Table App TableEntry
variantTable paramNames tests = mempty
  ++ Table.int "#" tableEntryRank
  ++ mconcat (map paramExtractor paramNames)
  ++ mconcat (map (\e@(Entity _ t) -> resultCell t (extractScore $ getTestReference e)) tests)
  ++ Table.widget "" variantStatusCellWidget

variantStatusCellWidget :: TableEntry -> WidgetFor App ()
variantStatusCellWidget entry = $(widgetFile "variant-status")
  where theVariantId = entityKey $ tableEntryVariant entry

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
  \(TableEntry (Entity _ s) (Entity _ v) (Entity u _) _ tagEnts paramEnts _ _) -> fragmentWithSubmissionTags
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

extractScore :: TestReference -> TableEntry -> Maybe Evaluation
extractScore k tableEntry = lookup k $ tableEntryMapping tableEntry

leaderboardTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
leaderboardTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ Table.text "submitter" (formatSubmitter . leaderboardUser . snd)
  ++ timestampCell "when" (submissionStamp . leaderboardBestSubmission . snd)
  ++ Table.text "ver." (formatVersion . leaderboardVersion . snd)
  ++ leaderboardDescriptionCell mauthId
  ++ mconcat (map (\e@(Entity _ t) -> resultCell t (extractScoreFromLeaderboardEntry (getTestReference e) . snd)) tests)
  ++ Table.int "×" (leaderboardNumberOfSubmissions . snd)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                       leaderboardBestSubmission e,
                                       leaderboardBestVariantId e,
                                       leaderboardBestVariant e,
                                       leaderboardUserId e,
                                       mauthId))

altLeaderboardTable :: Maybe UserId -> Text -> RepoScheme -> Repo -> [Entity Test] -> Table App (Int, LeaderboardEntry)
altLeaderboardTable mauthId challengeName repoScheme challengeRepo tests = mempty
  ++ Table.int "#" fst
  ++ leaderboardOnlyTagsCell mauthId
  ++ mconcat (map (\e@(Entity _ t) -> resultCell t (extractScoreFromLeaderboardEntry (getTestReference e) . snd)) tests)
  ++ statusCell challengeName repoScheme challengeRepo (\(_, e) -> (leaderboardBestSubmissionId e,
                                       leaderboardBestSubmission e,
                                       leaderboardBestVariantId e,
                                       leaderboardBestVariant e,
                                       leaderboardUserId e,
                                       mauthId))


extractScoreFromLeaderboardEntry :: TestReference -> LeaderboardEntry -> Maybe Evaluation
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

leaderboardOnlyTagsCell :: Maybe UserId -> Table App (a, LeaderboardEntry)
leaderboardOnlyTagsCell mauthId = Table.widget "tags" (
  \(_,entry) -> fragmentWithSubmissionTags ("" :: Text)
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

textLimited :: Int -> Text -> Text
textLimited limit t
  | l < limit = t
  | otherwise = (Data.Text.take limit t) <> "…"
  where l = length t

limitedTextCell :: Text -> Int -> Int -> (a -> Text) -> Table site a
limitedTextCell h softLimit hardLimit textFun = Table.widget h (
  \v -> [whamlet|<span title="#{textLimited hardLimit $ textFun v}"><tt>#{textLimited softLimit $ textFun v}</tt>|])

theLimitedTextCell :: Text -> (a -> Text) -> Table site a
theLimitedTextCell h textFun = limitedTextCell h softLimit hardLimit textFun
  where softLimit = 140
        hardLimit = 5 * softLimit


statusCellWidget :: Text -> RepoScheme -> Repo -> (SubmissionId, Submission, VariantId, Variant, UserId, Maybe UserId) -> WidgetFor App ()
statusCellWidget challengeName repoScheme challengeRepo (submissionId, submission, variantId, _, userId, mauthId) = do
  isReevaluable <- handlerToWidget $ runDB $ canBeReevaluated submissionId
  isVisible <- handlerToWidget $ runDB $ checkWhetherVisible submission userId mauthId
  $(widgetFile "submission-status")
    where commitHash = fromSHA1ToText $ submissionCommit submission
          isPublic = submissionIsPublic submission
          isOwner = (mauthId == Just userId)
          publicSubmissionBranch = getPublicSubmissionBranch submissionId
          maybeBrowsableUrl = if isPublic
                                then
                                  Just $ browsableGitRepoBranch repoScheme challengeRepo challengeName publicSubmissionBranch
                                else
                                  Nothing

getInfoLink :: Submission -> UserId -> Maybe UserId -> Maybe (Route App)
getInfoLink submission userId mauthId = if checkSimpleVisibility submission userId mauthId
                                        then Just $ QueryResultsR commitHash
                                        else Nothing
   where commitHash = fromSHA1ToText $ submissionCommit submission

-- sometimes we checker whether we got a teacher, but sometimes
-- fall back to a simpler check...
checkSimpleVisibility :: Submission -> UserId -> Maybe UserId -> Bool
checkSimpleVisibility submission userId mauthId = isPublic || isOwner
   where isPublic = submissionIsPublic submission
         isOwner = (mauthId == Just userId)

checkWhetherVisible :: (MonadIO m, BackendCompatible SqlBackend backend, PersistQueryRead backend, PersistUniqueRead backend)
                      => Submission -> Key User -> Maybe (Key User) -> ReaderT backend m Bool
checkWhetherVisible submission _ Nothing = return $ submissionIsPublic submission
checkWhetherVisible submission userId (Just seerId) = do
  let challengeId = submissionChallenge submission
  achvs <- E.select $ E.from $ \(achievement, course, participant, teacher) -> do
     E.where_ (achievement ^. AchievementChallenge E.==. E.val challengeId
               E.&&. achievement ^. AchievementCourse E.==. course ^. CourseId
               E.&&. participant ^. ParticipantUser E.==. E.val userId
               E.&&. participant ^. ParticipantCourse E.==. course ^. CourseId
               E.&&. teacher ^. TeacherUser E.==. E.val seerId
               E.&&. teacher ^. TeacherCourse E.==. course ^. CourseId)
     E.limit 2
     return ()
  let isTeacher = case achvs of
        [] -> False
        _  -> True
  return (isPublic || isOwner || isTeacher)
  where isPublic = submissionIsPublic submission
        isOwner = (seerId == userId)

getAuxSubmissionEnts :: TestReference -> [TableEntry] -> [(Key User, (User, [(Entity Submission, Entity Variant, Evaluation)]))]
getAuxSubmissionEnts testId evaluationMaps = map processEvaluationMap evaluationMaps
   where processEvaluationMap (TableEntry s v (Entity ui u) m _ _ _ _) = (ui, (u, case Map.lookup testId m of
                                                                                     Just e -> [(s, v, e)]
                                                                                     Nothing -> []))


compareMajorVersions ::  (Int, Int, Int) -> (Int, Int, Int) -> Ordering
compareMajorVersions (aM, _, _) (bM, _, _) = aM `compare` bM

compareVersions :: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
compareVersions (aM, aN, aP) (bM, bN, bP) = (aM `compare` bM)
                                            <> (aN `compare` bN)
                                            <> (aP `compare` bP)

getLeaderboardEntriesByCriterion :: (Ord a) => Int
                                 -> Key Challenge
                                 -> ((Entity Submission) -> Bool)
                                 -> ([(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))])
                                 -> (TableEntry -> [a])
                                 -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntriesByCriterion maxPriority challengeId condition preselector selector = do
  (evaluationMaps, tests) <- runDB $ getChallengeSubmissionInfos maxPriority condition (const True) preselector challengeId
  let mainTests = getMainTests tests
  let mainTestEnt = getMainTest tests
  let mainTestRef = getTestReference mainTestEnt
  let (Entity _ mainTest) = mainTestEnt
  let auxItems = concat
                 $ map (\i -> map (\s -> (s, [i])) (selector i))
                 $ filter (\entry -> member mainTestRef $ tableEntryMapping entry)
                 $ evaluationMaps
  let auxItemsMap = Map.fromListWith (++) auxItems
  let entryComparator a b =
        (compareMajorVersions (leaderboardVersion a) (leaderboardVersion b))
        <>
        ((compareResult mainTest) (evaluationScore $ leaderboardEvaluationMap a Map.! mainTestRef)
         (evaluationScore $ leaderboardEvaluationMap b Map.! mainTestRef))
        <>
        (compareVersions (leaderboardVersion a) (leaderboardVersion b))
  entries' <- mapM (toLeaderboardEntry challengeId mainTests)
             $ filter (\ll -> not (null ll))
             $ map snd
             $ Map.toList auxItemsMap
  let entries = DL.nubBy (\a b -> leaderboardBestVariantId a == leaderboardBestVariantId b)
                $ sortBy (flip entryComparator) entries'
  return (entries, (evaluationMaps, mainTests))


toLeaderboardEntry :: (Foldable t, YesodPersist site, PersistQueryRead (YesodPersistBackend site), PersistUniqueRead (YesodPersistBackend site), BaseBackend (YesodPersistBackend site) ~ SqlBackend) => Key Challenge -> [Entity Test] -> t TableEntry -> HandlerFor site LeaderboardEntry
toLeaderboardEntry challengeId tests ss = do
  let bestOne = DL.maximumBy submissionComparator ss
  let (TableEntry bestSubmission bestVariant user evals _ _ _ _) = bestOne
  let submissionId = entityKey bestSubmission
  tagEnts <- runDB $ getTags submissionId

  parameters <- runDB $ selectList [ParameterVariant ==. (entityKey bestVariant)] [Asc ParameterName]

  submission <- runDB $ get404 submissionId
  (Just (Entity _ version)) <- runDB $ getBy $ UniqueVersionByCommit $ submissionVersion submission

  let theVersion = (versionMajor version,
                    versionMinor version,
                    versionPatch version)

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
              leaderboardParams = map entityVal parameters,
              leaderboardVersion = theVersion
              }
     where mainTestEnt@(Entity _ mainTest) = getMainTest tests
           mainTestRef = getTestReference mainTestEnt
           submissionComparator (TableEntry _  _  _ em1 _ _ _ v1) (TableEntry _  _ _ em2 _ _ _ v2) =
             (compareMajorVersions v1 v2)
             <>
             (compareResult mainTest) (evaluationScore (em1 Map.! mainTestRef))
                                      (evaluationScore (em2 Map.! mainTestRef))
             <>
             (compareVersions v1 v2)

getLeaderboardEntries :: Int -> LeaderboardStyle -> Key Challenge -> Handler ([LeaderboardEntry], ([TableEntry], [Entity Test]))
getLeaderboardEntries maxPriority BySubmitter challengeId =
  getLeaderboardEntriesByCriterion maxPriority
                                   challengeId
                                   (const True)
                                   onlyTheBestVariant
                                   (\entry -> [entityKey $ tableEntrySubmitter entry])
getLeaderboardEntries maxPriority ByTag challengeId =
  getLeaderboardEntriesByCriterion maxPriority
                                   challengeId
                                   (const True)
                                   onlyTheBestVariant
                                   (noEmptyList . (map (entityKey . fst)) . tableEntryTagsInfo)
  where noEmptyList [] = [Nothing]
        noEmptyList l = map Just l

compareResult :: Test -> Maybe Double -> Maybe Double -> Ordering
compareResult test (Just x) (Just y) = (compareFun $ getMetricOrdering $ evaluationSchemeMetric $ testMetric test) x y
compareResult _ (Just _) Nothing = GT
compareResult _ Nothing (Just _) = LT
compareResult _ Nothing Nothing = EQ

onlyTheBestVariant :: [(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))]
onlyTheBestVariant = DL.nubBy (\(_, (Entity aid _, _)) (_, (Entity bid _, _)) -> aid == bid)
                     . (sortBy (\(r1, (_, Entity _ va)) (r2, (_, Entity _ vb)) -> (r1 `compare` r2)
                                                                                     `thenCmp`
                                                                    ((variantName va) `compare` (variantName vb))))
getChallengeSubmissionInfos :: (MonadIO m,
                               PersistQueryRead backend,
                               BackendCompatible SqlBackend backend,
                               PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
                              => Int
                                -> (Entity Submission -> Bool)
                                -> (Entity Variant -> Bool)
                                -> ([(Int, (Entity Submission, Entity Variant))] -> [(Int, (Entity Submission, Entity Variant))])
                                -> Key Challenge
                                -> ReaderT backend m ([TableEntry], [Entity Test])
getChallengeSubmissionInfos maxMetricPriority condition variantCondition preselector challengeId = do

  challenge <- get404 challengeId
  let commit = challengeVersion challenge

  tests' <- selectList [TestChallenge ==. challengeId, TestActive ==. True, TestCommit ==. commit] []
  let tests = filter (\t -> (evaluationSchemePriority $ testMetric $ entityVal t) <= maxMetricPriority) tests'
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
        $ preselector
        $ filter (\(_, (s, _)) -> condition s)
        $ map (\(rank, (_, sv)) -> (rank, sv))
        $ zip [1..]
        $ sortBy (\(s1, _) (s2, _) -> compareResult (entityVal mainTest) s2 s1)
        $ zip scores allSubmissionsVariants

  allTests <- selectList [] [Asc TestName]
  let testsMap = Map.fromList $ map (\(ent@(Entity testId _)) -> (testId, getTestReference ent)) allTests

  let allSubmissions = DL.nubBy (\(Entity a _) (Entity b _) -> a == b) $ map (\(_, (s, _)) -> s) allSubmissionsVariantsWithRanks
  subs <- mapM getBasicSubmissionInfo allSubmissions
  let submissionMap = Map.fromList subs
  -- testsMap and submissionMap are created to speed up getEvaluationMap

  evaluationMaps' <- mapM (getEvaluationMap testsMap submissionMap) allSubmissionsVariantsWithRanks
  let evaluationMaps = filter (variantCondition . tableEntryVariant) evaluationMaps'
  return (evaluationMaps, tests)

getScore :: (MonadIO m, BackendCompatible SqlBackend backend,
            PersistQueryRead backend, PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
           => Key Test -> Key Variant -> ReaderT backend m (Maybe Double)
getScore testId variantId = do
  evaluations <- E.select $ E.from $ \(out, evaluation, variant, submission) -> do
                  E.where_ (out ^. OutVariant E.==. E.val variantId
                            E.&&. variant ^. VariantId E.==. E.val variantId
                            E.&&. submission ^. SubmissionId E.==. variant ^. VariantSubmission
                            E.&&. out ^. OutTest E.==. E.val testId
                            E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum
                            -- all this complication here and with orderBy due
                            -- to the legacy issue with evaluation version sometimes missing
                            E.&&. (evaluation ^. EvaluationVersion E.==. E.just (submission ^. SubmissionVersion)
                                   E.||. E.isNothing (evaluation ^. EvaluationVersion))
                            E.&&. evaluation ^. EvaluationTest E.==. E.val testId)
                  E.orderBy [E.asc (evaluation ^. EvaluationScore)]
                  return evaluation
  return $ case evaluations of
             (e:_) -> evaluationScore $ entityVal e
             [] -> Nothing


data BasicSubmissionInfo = BasicSubmissionInfo {
  basicSubmissionInfoUser :: User,
  basicSubmissionInfoTagEnts :: [(Entity Tag, Entity SubmissionTag)],
  basicSubmissionInfoVersion :: Version }

getBasicSubmissionInfo :: (MonadIO m, PersistQueryRead backend,
                          PersistUniqueRead backend,
                          BaseBackend backend ~ SqlBackend)
                         => Entity Submission -> ReaderT backend m (SubmissionId, BasicSubmissionInfo)
getBasicSubmissionInfo (Entity submissionId submission) = do
  user <- get404 $ submissionSubmitter submission
  tagEnts <- getTags submissionId
  let versionHash = submissionVersion submission
  (Entity _ version) <- getBy404 $ UniqueVersionByCommit versionHash
  return $ (submissionId, BasicSubmissionInfo {
               basicSubmissionInfoUser = user,
               basicSubmissionInfoTagEnts = tagEnts,
               basicSubmissionInfoVersion = version })

getEvaluationMap :: (MonadIO m, PersistQueryRead backend, PersistUniqueRead backend, BaseBackend backend ~ SqlBackend)
                   => Map.Map TestId TestReference
                     -> Map.Map SubmissionId BasicSubmissionInfo
                     -> (Int, (Entity Submission, Entity Variant)) -> ReaderT backend m TableEntry
getEvaluationMap testsMap submissionsMap (rank, (s@(Entity submissionId submission), v@(Entity variantId _))) = do
  let submissionInfo = submissionsMap Map.! submissionId
  let user = basicSubmissionInfoUser submissionInfo
  let tagEnts = basicSubmissionInfoTagEnts submissionInfo
  let version = basicSubmissionInfoVersion submissionInfo

  outs <- selectList [OutVariant ==. variantId] [Asc OutId]
  let versionHash = submissionVersion submission
  maybeEvaluations <- mapM (\(Entity _ o) -> fetchTheEvaluation o versionHash) outs
  let evaluations = catMaybes maybeEvaluations
  let pairs = map (\(Entity _ e) -> (evaluationTest e, e)) evaluations
  let pairs' = map (\(testId, e) -> (testsMap Map.! testId, e)) pairs
  let m = Map.fromList pairs'

  parameters <- selectList [ParameterVariant ==. variantId] [Asc ParameterName]

  let major = versionMajor version
  let minor = versionMinor version
  let patch = versionPatch version

  return $ TableEntry s v (Entity (submissionSubmitter submission) user) m tagEnts parameters rank (major, minor, patch)
