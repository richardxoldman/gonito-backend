module Handler.Query where

import Import

import Handler.SubmissionView
import Handler.Shared
import Handler.TagUtils
import PersistSHA1

import Handler.Tables

import qualified Yesod.Table as Table

import Database.Persist.Sql

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import qualified Data.Text as T

import Data.List (nub)
import Data.List.Extra (groupOn)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Data.Conduit.SmartSource (lookForCompressedFiles)
import GEval.Core (GEvalSpecification(..), ResultOrdering(..))
import GEval.LineByLine (runLineByLineGeneralized, LineRecord(..))
import qualified Data.Conduit.List as CL
import System.FilePath (takeFileName)

rawCommitQuery :: (MonadIO m, RawSql a) => Text -> ReaderT SqlBackend m [a]
rawCommitQuery sha1Prefix =
  rawSql "SELECT ?? FROM submission WHERE is_public AND cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]

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
  submissions <- runDB $ case mauthId of
    Just (Entity authId _) -> rawSql "SELECT ?? FROM submission WHERE (is_public OR submitter = ?) AND cast(commit as text) like ?" [toPersistValue authId, PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
    Nothing -> rawCommitQuery sha1Prefix
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
        [eval] -> return $ formatTruncatedScore (testPrecision $ entityVal testEnt) (Just $ entityVal eval)
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
    Just (eval, testEnt) -> return $ formatTruncatedScore (testPrecision $ entityVal testEnt)
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


getViewVariantR :: VariantId -> Handler Html
getViewVariantR variantId = do
  mauthId <- maybeAuth
  variant <- runDB $ get404 variantId
  let theSubmissionId = variantSubmission variant
  theSubmission <- runDB $ get404 theSubmissionId

  ([entry], tests') <- runDB $ getChallengeSubmissionInfos 3
                                                          (\e -> entityKey e == theSubmissionId)
                                                          (\e -> entityKey e == variantId)
                                                          (submissionChallenge theSubmission)
  let tests = sortBy (flip testComparator) tests'

  if submissionIsPublic theSubmission || Just (submissionSubmitter theSubmission) == (entityKey <$> mauthId)
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

      defaultLayout $ do
        setTitle "Variant"
        $(widgetFile "view-variant")
    else
      error "Cannot access this submission variant"


outputEvaluationsTable :: TableEntry -> Table.Table App (Entity Test)
outputEvaluationsTable tableEntry = mempty
  ++ Table.text "Metric" (formatTestEvaluationScheme . entityVal)
  ++ Table.text "Score" (\test -> (formatTruncatedScore (testPrecision $ entityVal test)
                                  $ extractScore (getTestReference test) tableEntry))


paramsTable :: Table.Table App Parameter
paramsTable = mempty
  ++ Table.text "Parameter" parameterName
  ++ Table.text "Value" parameterValue

viewOutput :: TableEntry -> [Entity Test] -> (SHA1, Text) -> WidgetFor App ()
viewOutput entry tests (outputHash, testSet) = do
  let tests'@(mainTest:_) = filter (\e -> (testName $ entityVal e) == testSet) tests
  let outputSha1AsText = fromSHA1ToText $ outputHash

  mRepoDir <- handlerToWidget $ justGetSubmissionRepoDir (entityKey $ tableEntrySubmission entry)
  let variant = variantName $ entityVal $ tableEntryVariant entry

  let theStamp = submissionStamp $ entityVal $ tableEntrySubmission entry
  let isPublic = submissionIsPublic $ entityVal $ tableEntrySubmission entry
  challenge <- handlerToWidget $ runDB $ get404 $ submissionChallenge $ entityVal $ tableEntrySubmission entry
  let isNonSensitive = challengeSensitive challenge == Just False

  let shouldBeShown = not ("test-" `isInfixOf` testSet) && isPublic && isNonSensitive

  let mainMetric = testMetric $ entityVal mainTest

  mResult <-
    if shouldBeShown
      then
        case mRepoDir of
               Just repoDir -> do
                 outFile' <- liftIO $ lookForCompressedFiles (repoDir </> (T.unpack variant) <.> "tsv")
                 let outFile = takeFileName outFile'

                 let spec = GEvalSpecification {
                       gesOutDirectory = repoDir,
                       gesExpectedDirectory = Nothing,
                       gesTestName = (T.unpack testSet),
                       gesSelector = Nothing,
                       gesOutFile = outFile,
                       gesExpectedFile = "expected.tsv",
                       gesInputFile = "in.tsv",
                       gesMetrics = [mainMetric],
                       gesPrecision = Nothing,
                       gesTokenizer = Nothing,
                       gesGonitoHost = Nothing,
                       gesToken = Nothing,
                       gesGonitoGitAnnexRemote = Nothing,
                       gesReferences = Nothing }

                 result <- liftIO $ runLineByLineGeneralized FirstTheWorst spec (\_ -> CL.take 20)

                 return $ Just $ zip [1..] result
               Nothing -> return Nothing
      else
        return Nothing
  $(widgetFile "view-output")

lineByLineTable :: Entity Test -> UTCTime -> Table.Table App (Int, LineRecord)
lineByLineTable (Entity testId test) theStamp = mempty
  ++ Table.int "#" fst
  ++ theLimitedTextCell "input" (((\(LineRecord inp _ _ _ _) -> inp) . snd))
  ++ theLimitedTextCell "expected output" ((\(LineRecord _ expected _ _ _) -> expected) . snd)
  ++ theLimitedTextCell "actual output" ((\(LineRecord _ _ out _ _) -> out) . snd)
  ++ resultCell test (fakeEvaluation . (\(LineRecord _ _ _ _ score) -> score) . snd)
  where fakeEvaluation score = Just $ Evaluation {
          evaluationTest = testId,
          evaluationChecksum = testChecksum test,
          evaluationScore = Just score,
          evaluationErrorMessage = Nothing,
          evaluationStamp = theStamp,
          evaluationVersion = Nothing }

resultTable :: Entity Submission -> WidgetFor App ()
resultTable (Entity submissionId submission) = do
  (tableEntries, tests) <- handlerToWidget
                          $ runDB
                          $ getChallengeSubmissionInfos 2
                                                        (\s -> entityKey s == submissionId)
                                                        (const True)
                                                        (submissionChallenge submission)
  let paramNames =
        nub
        $ map (parameterName . entityVal)
        $ concat
        $ map tableEntryParams tableEntries

  let resultId = show $ fromSqlKey submissionId
  let jsSelector = String $ T.pack ("#t" ++ resultId ++ " > table")

  let delta = Number $ fromIntegral ((length paramNames) + 1)
  let higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests

  $(widgetFile "result-table")


submissionHeader :: FullSubmissionInfo -> Maybe Text -> WidgetFor App ()
submissionHeader submission mVariantName =
  $(widgetFile "submission-header")
    where commitSha1AsText = fromSHA1ToText $ submissionCommit $ fsiSubmission submission
          submitter = formatSubmitter $ fsiUser submission
          publicSubmissionBranch = getPublicSubmissionBranch $ fsiSubmissionId submission
          publicSubmissionRepo = getReadOnlySubmissionUrl (fsiScheme submission) (fsiChallengeRepo submission) $ challengeName $ fsiChallenge submission
          browsableUrl = browsableGitRepoBranch (fsiScheme submission) (fsiChallengeRepo submission) (challengeName $ fsiChallenge submission) publicSubmissionBranch
          stamp = T.pack $ show $ submissionStamp $ fsiSubmission submission

queryResult :: FullSubmissionInfo -> WidgetFor App ()
queryResult submission = do
  $(widgetFile "query-result")

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing
