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

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

rawCommitQuery :: (MonadIO m, RawSql a) => Text -> ReaderT SqlBackend m [a]
rawCommitQuery sha1Prefix =
  rawSql "SELECT ?? FROM submission WHERE is_public AND cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]

findSubmissions :: Text -> Handler [FullSubmissionInfo]
findSubmissions sha1Prefix = do
  mauthId <- maybeAuth
  submissions <- runDB $ case mauthId of
    Just (Entity authId _) -> rawSql "SELECT ?? FROM submission WHERE (is_public OR submitter = ?) AND cast(commit as text) like ?" [toPersistValue authId, PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
    Nothing -> rawCommitQuery sha1Prefix
  mapM getFullInfo submissions

getApiTxtScoreR :: Text -> Handler Text
getApiTxtScoreR sha1Prefix = do
  submissions <- runDB $ rawCommitQuery sha1Prefix
  case submissions of
    [submission] -> doGetScore submission
    [] -> return "NONE"
    _ -> return "AMBIGUOUS ARGUMENT"

doGetScore :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistUniqueRead (YesodPersistBackend site), BackendCompatible SqlBackend (YesodPersistBackend site), YesodPersist site, PersistQueryRead (YesodPersistBackend site)) => Entity Submission -> HandlerFor site Text
doGetScore submission = do
  let challengeId = submissionChallenge $ entityVal submission
  tests <- runDB $ selectList [TestChallenge ==. challengeId] []
  let mainTest = getMainTest tests
  let mainTestId = entityKey mainTest
  let submissionId = entityKey submission

  evals <- runDB $ E.select
                   $ E.from $ \(out, evaluation, variant) -> do
                     E.where_ (variant ^. VariantSubmission E.==. E.val submissionId
                               E.&&. out ^. OutVariant E.==. variant ^. VariantId
                               E.&&. out ^. OutTest E.==. E.val mainTestId
                               E.&&. evaluation ^. EvaluationTest E.==. E.val mainTestId
                               E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum)
                     E.orderBy []
                     return (evaluation)

  case evals of
    [eval] -> return $ formatTruncatedScore (testPrecision $ entityVal mainTest) (Just $ entityVal eval)
    _ -> return "NONE"

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
  submissions <- findSubmissions query
  defaultLayout $ do
    setTitle "query results"
    $(widgetFile "query-results")

resultTable :: Entity Submission -> WidgetFor App ()
resultTable (Entity submissionId submission) = do
  (tableEntries, tests) <- handlerToWidget $ runDB $ getChallengeSubmissionInfos (\s -> entityKey s == submissionId)
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

queryResult :: FullSubmissionInfo -> WidgetFor App ()
queryResult submission = do
  $(widgetFile "query-result")
    where commitSha1AsText = fromSHA1ToText $ submissionCommit $ fsiSubmission submission
          submitter = formatSubmitter $ fsiUser submission
          publicSubmissionBranch = getPublicSubmissionBranch $ fsiSubmissionId submission
          publicSubmissionRepo = getReadOnlySubmissionUrl (fsiScheme submission) (fsiChallengeRepo submission) $ challengeName $ fsiChallenge submission
          browsableUrl = browsableGitRepoBranch (fsiScheme submission) (fsiChallengeRepo submission) (challengeName $ fsiChallenge submission) publicSubmissionBranch
          stamp = T.pack $ show $ submissionStamp $ fsiSubmission submission

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing
