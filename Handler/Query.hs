{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Query where

import Import

import Handler.SubmissionView
import Handler.Shared
import Handler.TagUtils
import PersistSHA1

import Data.Diff

import Handler.Tables

import Text.Blaze

import qualified Yesod.Table as Table

import Database.Persist.Sql

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Data.Maybe (fromJust)

import qualified Data.Text as T

import Data.List (nub, (!!))
import Data.List.Extra (groupOn)
import qualified Data.Map.Lazy as LM

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Data.Conduit.SmartSource (lookForCompressedFiles)
import GEval.Core (GEvalSpecification(..), GEvalOptions(..), ResultOrdering(..))
import GEval.LineByLine (runLineByLineGeneralized, runDiffGeneralized, LineRecord(..))
import GEval.Common (FormattingOptions(..), MetricValue)
import GEval.OptionsParser (readOptsFromConfigFile)
import qualified Data.Conduit.List as CL
import System.FilePath (takeFileName)
import System.Directory (makeAbsolute)

import Data.SplitIntoCrossTabs

rawCommitQuery :: (MonadIO m, RawSql a) => Text -> ReaderT SqlBackend m [a]
rawCommitQuery sha1Prefix =
  rawSql "SELECT ?? FROM submission WHERE cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]

rawOutQuery :: (MonadIO m, RawSql a) => Text -> ReaderT SqlBackend m [a]
rawOutQuery sha1Prefix =
  rawSql "SELECT ?? FROM out WHERE cast(checksum as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]

groupBySecond :: Eq b => [(FullSubmissionInfo, b)] -> [(FullSubmissionInfo, [b])]
groupBySecond lst = map putOut $ groupOn (fsiSubmissionId . fst) lst
  where putOut ((ha, hb):t) = (ha, hb:nub (map snd t))
        putOut [] = error "should not be here"

findSubmissions :: Text -> Handler [(FullSubmissionInfo, [SHA1])]
findSubmissions sha1Prefix = do
  mauthId <- maybeAuth
  allSubmissions <- runDB $ rawCommitQuery sha1Prefix
  submissions <- filterM (\sub -> runDB $ checkWhetherVisible (entityVal sub) (entityKey <$> mauthId)) allSubmissions
  justSubmissions' <- mapM getFullInfo submissions
  let justSubmissions = map (\s -> (s, [])) justSubmissions'

  outs <- runDB $ rawOutQuery sha1Prefix
  submissionsByOuts <- mapM fetchSubmissionByOut outs

  return (justSubmissions ++ groupBySecond submissionsByOuts)

fetchSubmissionByOut :: Entity Out -> HandlerFor App (FullSubmissionInfo, SHA1)
fetchSubmissionByOut (Entity _ out) = do
  variant <- runDB $ get404 $ outVariant out
  let theSubmissionId = variantSubmission variant
  theSubmission <- runDB $ get404 theSubmissionId
  let theSubmissionEnt = Entity theSubmissionId theSubmission
  fsi <- getFullInfo theSubmissionEnt
  return (fsi, outChecksum out)

getApiTxtScoreR :: Text -> Handler Text
getApiTxtScoreR query =
  if T.null post
  then getApiTxtScore Nothing pre
  else getApiTxtScore (Just $ T.tail post) pre
  where (pre, post) = T.breakOn "-" query

getApiTxtScore :: Maybe Text -> Text -> Handler Text
getApiTxtScore mMetricName sha1Prefix = do
  submissions <- findSubmissions sha1Prefix
  case submissions of
    [] -> return noneMessage
    ((fsi, _):_) -> case submissions of
      [(_, [])] -> doGetScore mMetricName (Entity (fsiSubmissionId fsi)
                                                 (fsiSubmission fsi))
      _ -> do
        let hashes = nub $ concat $ map snd submissions
        case hashes of
          [h] -> doGetScoreForOut mMetricName
                                 (Entity (fsiSubmissionId fsi)
                                         (fsiSubmission fsi))
                                 h
          [] -> return noneMessage
          _ -> return ambiguousArgumentMessage
  where ambiguousArgumentMessage = "AMBIGUOUS ARGUMENT"
        noneMessage = "NONE"

doGetScore :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistUniqueRead (YesodPersistBackend site), BackendCompatible SqlBackend (YesodPersistBackend site), YesodPersist site, PersistQueryRead (YesodPersistBackend site)) => Maybe Text -> Entity Submission -> HandlerFor site Text
doGetScore mMetricName submission = do
  let challengeId = submissionChallenge $ entityVal submission

  mTestEnt <- runDB $ fetchTestByName mMetricName challengeId
  case mTestEnt of
    Just testEnt -> do
      let theTestId = entityKey testEnt

      let submissionId = entityKey submission

      evals <- runDB $ E.select
                    $ E.from $ \(out, evaluation, variant) -> do
                      E.where_ (variant ^. VariantSubmission E.==. E.val submissionId
                                E.&&. out ^. OutVariant E.==. variant ^. VariantId
                                E.&&. out ^. OutTest E.==. E.val theTestId
                                E.&&. evaluation ^. EvaluationTest E.==. E.val theTestId
                                E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum)
                      E.orderBy []
                      return (evaluation)

      case evals of
        [eval] -> return $ formatTruncatedScore (getTestFormattingOpts $ entityVal testEnt) (Just $ entityVal eval)
        _ -> return "NONE"
    Nothing -> return "NONE"

doGetScoreForOut :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistUniqueRead (YesodPersistBackend site), BackendCompatible SqlBackend (YesodPersistBackend site), YesodPersist site, PersistQueryRead (YesodPersistBackend site)) => Maybe Text -> Entity Submission -> SHA1 -> HandlerFor site Text
doGetScoreForOut mMetricName submission sha1code = do
  let submissionId = entityKey submission

  evals <- runDB $ E.select
                $ E.from $ \(out, evaluation, variant, test, version) -> do
                  E.where_ (variant ^. VariantSubmission E.==. E.val submissionId
                            E.&&. out ^. OutVariant E.==. variant ^. VariantId
                            E.&&. out ^. OutTest E.==. test ^. TestId
                            E.&&. evaluation ^. EvaluationTest E.==. test ^. TestId
                            E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum
                            E.&&. out ^. OutChecksum E.==. E.val sha1code
                            E.&&. (evaluation ^. EvaluationVersion E.==. E.just (version ^. VersionCommit)))
                  E.orderBy [E.asc (test ^. TestPriority),
                             E.desc (version ^. VersionMajor),
                             E.desc (version ^. VersionMinor),
                             E.desc (version ^. VersionPatch)]
                  return (evaluation, test)

  let evalSelected = case evals of
        [] -> Nothing
        ((eval, test):_)  -> case mMetricName of
                              Nothing -> Just (eval, test)
                              Just mn -> find (\(_, t) -> formatTestEvaluationScheme (entityVal t) == mn) evals
  case evalSelected of
    Nothing -> return "None"
    Just (eval, testEnt) -> return $ formatTruncatedScore (getTestFormattingOpts $ entityVal testEnt)
                                                         (Just $ entityVal eval)


getQueryFormR :: Handler Html
getQueryFormR = do
  (formWidget, formEnctype) <- generateFormPost queryForm
  defaultLayout $ do
      setTitle "Searching for submissions"
      $(widgetFile "query-form")

postQueryFormR :: Handler Html
postQueryFormR = do
    ((result, formWidget), formEnctype) <- runFormPost queryForm
    case result of
      FormSuccess query -> processQuery query
      _ -> defaultLayout $ do
        setTitle "Searching for submissions"
        $(widgetFile "query-form")

getQueryResultsR :: Text -> Handler Html
getQueryResultsR = processQuery

isFullQuery :: Text -> Bool
isFullQuery query = length query == 40

processQuery :: Text -> Handler Html
processQuery query = do
  submissions' <- findSubmissions query
  let submissions = map fst submissions'
  defaultLayout $ do
    setTitle "query results"
    $(widgetFile "query-results")

priorityLimitForViewVariant :: Int
priorityLimitForViewVariant = 4

getViewVariantDiffR :: VariantId -> VariantId -> TestId -> Handler Html
getViewVariantDiffR oldVariantId newVariantId testId = do
  doViewVariantTestR (TwoThings oldVariantId newVariantId) testId

getViewVariantTestR :: VariantId -> TestId -> Handler Html
getViewVariantTestR variantId testId = do
  doViewVariantTestR (OneThing variantId) testId

data ViewVariantData = ViewVariantData {
  viewVariantDataFullSubmissionInfo :: (FullSubmissionInfo, Maybe Text),
  viewVariantDataTableEntry :: TableEntry,
  viewVariantDataTests :: [Entity Test],
  viewVariantDataOuts :: [(SHA1, Text)]
  }

fetchViewVariantData :: VariantId -> Handler ViewVariantData
fetchViewVariantData variantId = do
  mauthId <- maybeAuth
  variant <- runDB $ get404 variantId
  let theSubmissionId = variantSubmission variant
  theSubmission <- runDB $ get404 theSubmissionId

  ([entry], tests') <- runDB $ getChallengeSubmissionInfos priorityLimitForViewVariant
                                                          (\e -> entityKey e == theSubmissionId)
                                                          (\e -> entityKey e == variantId)
                                                          id
                                                          (submissionChallenge theSubmission)
  let tests = sortBy (flip testComparator) tests'

  isViewable <- runDB $ checkWhetherVisible theSubmission (entityKey <$> mauthId)

  if isViewable
    then
     do
      fullSubmissionInfo <- getFullInfo (Entity theSubmissionId theSubmission)

      testOutputs <- runDB $ E.select
                          $ E.from $ \(out, test) -> do
                            E.where_ (out ^. OutTest E.==. test ^. TestId
                                      E.&&. out ^. OutVariant E.==. E.val variantId)
                            E.orderBy []
                            return (out, test)

      let outputs =
            sortBy (\a b -> ((snd b) `compare` (snd a)))
            $ nub
            $ map (\(out, test) -> (outChecksum $ entityVal out, testName $ entityVal test)) testOutputs

      return $ ViewVariantData (fullSubmissionInfo, Just $ variantName variant) entry tests outputs
    else
      error "Cannot access this submission variant"


instance Diffable SHA1 where
  type DiffSettings SHA1 = ()
  type DiffResult SHA1 = Diff SHA1
  single u = OneThing u
  diff _ old new
    | old == new = OneThing new
    | otherwise = TwoThings old new

postCompareFormR :: VariantId -> TestId -> Handler Html
postCompareFormR variantId testId = do
    ((result, _), _) <- runFormPost outQueryForm
    case result of
      FormSuccess outQuery -> do
        (out:_) <- runDB $ rawOutQuery outQuery
        let otherVariantId = outVariant $ entityVal out
        doViewVariantTestR (TwoThings otherVariantId variantId) testId

nullSHA1 :: SHA1
nullSHA1 = fromTextToSHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"

doViewVariantTestR :: Diff VariantId -> TestId -> Handler Html
doViewVariantTestR variantId testId = do
  testSelected <- runDB $ get404 testId
  let testSelectedEnt = Entity testId testSelected

  variantInfos <- mapM (fetchViewVariantData) variantId
  let fullSubmissionInfo = viewVariantDataFullSubmissionInfo <$> variantInfos
  let entry = viewVariantDataTableEntry <$> variantInfos
  let tests' = viewVariantDataTests <$> variantInfos
  let outputs' = viewVariantDataOuts <$> variantInfos


  let testIds = map fst $ runDiff () $ fmap (map entityKey) tests'
  testEnts <- mapM (runDB . get404) testIds
  let tests = map (\(i,e) -> Entity i e) $ zip testIds testEnts
  let outputs :: [(Diff SHA1, Text)] =
        sortBy (\a b -> ((snd b) `compare` (snd a)))
        $ map swap $ LM.toList $ runDiff (nullSHA1, ()) $ fmap (LM.fromList . map swap) outputs'

  (formWidget, formEnctype) <- generateFormPost outQueryForm

  defaultLayout $ do
    setTitle "Variant"
    $(widgetFile "view-variant")

mergeEntryParams :: Diff [Parameter] -> [(Text, Diff Text)]
mergeEntryParams (OneThing u) = map (\(Parameter _ name val) -> (name, OneThing val)) u
mergeEntryParams (TwoThings old new) = LM.toList $ diff ("", ()) oldMap newMap
   where oldMap = mapify old
         newMap = mapify new
         mapify l = LM.fromList $ map (\(Parameter _ name val) -> (name, val)) l

getViewVariantR :: VariantId -> Handler Html
getViewVariantR variantId = do
  variant <- runDB $ get404 variantId
  let theSubmissionId = variantSubmission variant
  theSubmission <- runDB $ get404 theSubmissionId

  (_, tests') <- runDB $ getChallengeSubmissionInfos priorityLimitForViewVariant
                                                    (\e -> entityKey e == theSubmissionId)
                                                    (\e -> entityKey e == variantId)
                                                    id
                                                    (submissionChallenge theSubmission)
  let (mainTest:_) = sortBy (flip testComparator) tests'
  getViewVariantTestR variantId (entityKey mainTest)

linkedWithAnchor :: (Text.Blaze.ToMarkup a1, Text.Blaze.ToMarkup a2)
                   => Text -> (t -> a2) -> (t -> Route site) -> (t -> a1) -> Table.Table site t
linkedWithAnchor h propFunc routeFunc anchorFunc =
  Table.widget h (
    \v -> [whamlet|<a href=@{routeFunc v}\\##{anchorFunc v}>#{propFunc v}|])

getVariantTestLink :: Diff VariantId -> TestId -> Route App
getVariantTestLink (OneThing u) testId = ViewVariantTestR u testId
getVariantTestLink (TwoThings old new) testId = ViewVariantDiffR old new testId

crossTableDefinition :: Diff VariantId -> TableWithValues (Entity Test, Diff Text) -> Table.Table App (Text, [(Entity Test, Diff Text)])
crossTableDefinition variantId (TableWithValues (headerH : headerR) _) = mempty
  ++ Table.text headerH fst
  ++ mconcat (map (\(ix, h) -> linkedWithAnchor h
                                               (snd . (!! ix) . snd)
                                               ((\(e, _) -> getVariantTestLink variantId (entityKey e)) . (!! ix) . snd)
                                               (("worst-items-" <>) . testName . entityVal . fst . (!! ix) . snd))
               $ zip [0..] headerR)
crossTableDefinition _ _ = error $ "cross-tab of an unexpected size"

crossTableBody :: TableWithValues (Entity Test, Diff Text) -> [(Text, [(Entity Test, Diff Text)])]
crossTableBody (TableWithValues _ rows) = rows

paramsTable :: Table.Table App (Text, Diff Text)
paramsTable = mempty
  ++ Table.text "Parameter" fst
  ++ Table.widget "Value" ((\t -> [whamlet|#{t}|]) . snd)

viewOutput :: Diff TableEntry -> [Entity Test] -> (SHA1, Text) -> WidgetFor App ()
viewOutput entry tests (outputHash, testSet) = do
  let (mainTest:_) = filter (\e -> (testName $ entityVal e) == testSet) tests
  viewOutputWithNonDefaultTestSelected entry tests mainTest (OneThing outputHash, testSet)

maximumNumberOfItemsToBeShown :: Int
maximumNumberOfItemsToBeShown = 40

getOut :: Maybe UserId -> TableEntry -> WidgetFor App (Maybe (FilePath, FilePath))
getOut mauthId entry = do
  let variant = variantName $ entityVal $ tableEntryVariant entry

  isViewable <- handlerToWidget $ runDB $ checkWhetherVisible (entityVal $ tableEntrySubmission entry) mauthId
  if isViewable
   then
    do
     mRepoDir <- handlerToWidget $ justGetSubmissionRepoDir $ entityKey $ tableEntrySubmission entry
     case mRepoDir of
       Just repoDir -> do
         outFilePath <- liftIO $ lookForCompressedFiles (repoDir </> (T.unpack variant) <.> "tsv")
         return $ Just (repoDir, outFilePath)
       Nothing -> return Nothing
   else
    do
     return Nothing

data DiffLineRecord = DiffLineRecord Text Text (Diff (Text, MetricValue)) Word32
                      deriving (Show)

getUniLineRecord :: LineRecord -> DiffLineRecord
getUniLineRecord (LineRecord inp expect out lineNo val) = DiffLineRecord inp expect (OneThing (out, val)) lineNo

getBiLineRecord :: (LineRecord, LineRecord) -> DiffLineRecord
getBiLineRecord ((LineRecord oldInp oldExp oldOut oldLineNo oldVal), (LineRecord newInp newExp newOut newLineNo newVal))
  | oldInp == newInp && oldExp == newExp && oldLineNo == newLineNo =  DiffLineRecord newInp
                                                                                newExp
                                                                                (TwoThings (oldOut, oldVal)
                                                                                           (newOut, newVal))
                                                                                newLineNo
  | otherwise = error "inconsistent line records when diffing"


getScoreFromDiff :: DiffLineRecord -> MetricValue
getScoreFromDiff (DiffLineRecord _ _ (OneThing (_, s)) _) = s
getScoreFromDiff (DiffLineRecord _ _ (TwoThings (_, oldS) (_, newS)) _) = newS - oldS


viewOutputWithNonDefaultTestSelected :: Diff TableEntry
                                       -> [Entity Test]
                                       -> Entity Test
                                       -> (Diff SHA1, Text)
                                       -> WidgetFor App ()
viewOutputWithNonDefaultTestSelected entry tests mainTest (outputHash, testSet) = do
  let tests' = filter (\e -> (testName $ entityVal e) == testSet) tests

  mauthId <- maybeAuthId

  let outputSha1AsText = fromSHA1ToText $ current outputHash

  let variantId = entityKey <$> tableEntryVariant <$> entry

  let theStamp = submissionStamp $ entityVal $ tableEntrySubmission $ current entry
  challenge <- handlerToWidget $ runDB $ get404 $ submissionChallenge $ entityVal $ tableEntrySubmission $ current entry
  let isNonSensitive = challengeSensitive challenge == Just False

  let shouldBeShown = not ("test-" `isInfixOf` testSet) && isNonSensitive

  let mainMetric = testMetric $ entityVal mainTest

  let testLabels = map (formatTestEvaluationScheme . entityVal) tests'
  let mapping = LM.fromList $ map (\test -> (formatTestEvaluationScheme $ entityVal test,
                                            (test,
                                             (formatTruncatedScore (getTestFormattingOpts $ entityVal test)
                                              <$> extractScore (getTestReference test) <$> entry)))) tests'
  let crossTables = splitIntoTablesWithValues "Metric" "Score" mapping testLabels

  mResult <-
    if shouldBeShown
      then
       do
        outPaths <- mapM (getOut mauthId) entry
        case current outPaths of
          Just _ -> do
            let repoDir = fst <$> fromJust <$> outPaths
            let outFilePath = snd <$> fromJust <$> outPaths
            let outFile = takeFileName $ current outFilePath

            let testName = T.unpack testSet

            Right opts <- liftIO $ readOptsFromConfigFile [] (current repoDir </> "config.txt")

            let spec = GEvalSpecification {
                  gesOutDirectory = current repoDir,
                  gesExpectedDirectory = Nothing,
                  gesTestName = testName,
                  gesSelector = Nothing,
                  gesOutFile = outFile,
                  gesAltOutFiles = Nothing,
                  gesExpectedFile = "expected.tsv",
                  gesInputFile = "in.tsv",
                  gesMetrics = [mainMetric],
                  gesFormatting = FormattingOptions {
                      decimalPlaces = Nothing,
                      asPercentage = False },
                  gesTokenizer = Nothing,
                  gesGonitoHost = Nothing,
                  gesToken = Nothing,
                  gesGonitoGitAnnexRemote = Nothing,
                  gesReferences = Nothing,
                  gesBootstrapResampling = Nothing,
                  gesInHeader = gesInHeader $ geoSpec opts,
                  gesOutHeader = gesOutHeader $ geoSpec opts,
                  gesShowPreprocessed = True }

            case outPaths of
              OneThing _ -> do
                result <- liftIO $ runLineByLineGeneralized FirstTheWorst
                                                       spec
                                                       (\_ -> CL.take maximumNumberOfItemsToBeShown)
                return $ Just $ zip [1..] $ map getUniLineRecord result
              TwoThings (Just (oldRepoDir, oldOutFilePath)) _ -> do
                absOldOutFilePath <- liftIO $ makeAbsolute (oldRepoDir </> testName </> (takeFileName oldOutFilePath))
                result <- liftIO $ runDiffGeneralized FirstTheWorst
                                                     absOldOutFilePath
                                                     spec
                                                     (\_ -> CL.take maximumNumberOfItemsToBeShown)
                return $ Just $ zip [1..] $ map getBiLineRecord result
          Nothing -> return Nothing
      else
        return Nothing
  $(widgetFile "view-output")

lineByLineTable :: Entity Test -> UTCTime -> Table.Table App (Int, DiffLineRecord)
lineByLineTable (Entity testId test) theStamp = mempty
  ++ Table.int "#" fst
  ++ theLimitedTextCell "input" (((\(DiffLineRecord inp _ _ _) -> inp) . snd))
  ++ theLimitedTextCell "expected output" ((\(DiffLineRecord _ expected _ _) -> expected) . snd)
  ++ theLimitedDiffTextCell "actual output" (fmap fst . (\(DiffLineRecord _ _ out _) -> out) . snd)
  ++ resultCell test (fakeEvaluation . getScoreFromDiff . snd)
  where fakeEvaluation score = Just $ Evaluation {
          evaluationTest = testId,
          evaluationChecksum = testChecksum test,
          evaluationScore = Just score,
          evaluationErrorBound = Nothing,
          evaluationErrorMessage = Nothing,
          evaluationStamp = theStamp,
          evaluationVersion = Nothing }

resultTable :: Entity Submission -> WidgetFor App ()
resultTable (Entity submissionId submission) = do
  (tableEntries, tests') <- handlerToWidget
                          $ runDB
                          $ getChallengeSubmissionInfos 2
                                                        (\s -> entityKey s == submissionId)
                                                        (const True)
                                                        id
                                                        (submissionChallenge submission)

  let paramNames =
        nub
        $ map (parameterName . entityVal)
        $ concat
        $ map tableEntryParams tableEntries

  let maximumNumberOfColumns = 10

  let tests = adjustNumberOfColumnsShown (maximumNumberOfColumns - length paramNames) tests'

  let resultId = show $ fromSqlKey submissionId
  let jsSelector = String $ T.pack ("#t" ++ resultId ++ " > table")

  let delta = Number $ fromIntegral ((length paramNames) + 1)
  let higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests

  $(widgetFile "result-table")

adjustNumberOfColumnsShown :: Int -> [Entity Test] -> [Entity Test]
adjustNumberOfColumnsShown maximumNumberOfColumns tests = adjustNumberOfColumnsShown' (max maximumNumberOfColumns minimumNumberOfTests) tests
  where adjustNumberOfColumnsShown' maximumNumberOfColumns' tests'
          | length tests <= maximumNumberOfColumns' = tests'
          | otherwise = let filteredTests = filter (\t -> not ("dev" `isInfixOf` (testName $ entityVal t))) tests'
                        in if null filteredTests
                            then tests'
                            else
                             if length filteredTests <= maximumNumberOfColumns'
                             then filteredTests
                             else take maximumNumberOfColumns' filteredTests

        minimumNumberOfTests = 2

data GitServer = Gogs | GitLab
  deriving (Eq, Show)

guessGitServer :: Text -> Maybe GitServer
guessGitServer bareUrl
  | "git.wmi.amu.edu.pl" `isPrefixOf` bareUrl = Just Gogs
  | "gitlab." `isPrefixOf` bareUrl = Just GitLab
  | "git." `isPrefixOf` bareUrl = Just GitLab
  | otherwise = Nothing

getHttpLink :: Repo -> Maybe (Text, Text)
getHttpLink repo = case guessGitServer bareUrl of
  Just Gogs -> Just (convertToHttpLink bareUrl, "/src/" <> branch)
  Just GitLab -> Just (convertToHttpLink bareUrl, "/-/tree/" <> branch)
  Nothing -> Nothing
  where bareUrl = T.replace "git@" "" url
        url = repoUrl repo
        branch = repoBranch repo
        convertToHttpLink = ("https://" <>) . (T.replace ":" "/") . (T.replace ".git" "")

submissionHeader :: Diff (FullSubmissionInfo, Maybe Text) -> WidgetFor App ()
submissionHeader param =
  $(widgetFile "submission-header")
    where variantSettings = ("out", ())
          submission = fst <$> param
          mVariantName = snd <$> param
          commitSha1AsText = fromSHA1ToText <$> submissionCommit <$> fsiSubmission <$> submission
          submitter = formatSubmitter <$> fsiUser <$> submission
          publicSubmissionBranch = getPublicSubmissionBranch <$> fsiSubmissionId <$> submission
          publicSubmissionRepo = submissionToSubmissionUrl <$> submission
          browsableUrl = submissionToBrowsableUrl <$> submission
          stamp = T.pack <$> show <$> submissionStamp <$> fsiSubmission <$> submission

          submissionToSubmissionUrl submission' = getReadOnlySubmissionUrl (fsiScheme submission') (fsiChallengeRepo submission') $ challengeName $ fsiChallenge submission'
          submissionToBrowsableUrl submission' = browsableGitRepoBranch (fsiScheme submission') (fsiChallengeRepo submission') (challengeName $ fsiChallenge submission') (getPublicSubmissionBranch $ fsiSubmissionId submission')


queryResult :: FullSubmissionInfo -> WidgetFor App ()
queryResult submission = do
  $(widgetFile "query-result")

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing

outQueryForm :: Form Text
outQueryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgOutSha1) Nothing
