module Handler.Query where

import Import

import Handler.Tables (formatSubmitter)
import PersistSHA1

import Database.Persist.Sql
import Data.Text as T(pack)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

data FullSubmissionInfo = FullSubmissionInfo {
  fsiSubmission :: Submission,
  fsiUser :: User,
  fsiRepo :: Repo }

getFullInfo :: Entity Submission -> Handler FullSubmissionInfo
getFullInfo (Entity submissionId submission) = do
  repo <- runDB $ get404 $ submissionRepo submission
  user <- runDB $ get404 $ submissionSubmitter submission
  return $ FullSubmissionInfo {
    fsiSubmission = submission,
    fsiUser = user,
    fsiRepo = repo }

findSubmissions :: Text -> Handler [FullSubmissionInfo]
findSubmissions sha1Prefix = do
  submissions <- runDB $ rawSql "SELECT ?? FROM  submission WHERE cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
  mapM getFullInfo submissions

getQueryFormR :: Handler Html
getQueryFormR = do
  (formWidget, formEnctype) <- generateFormPost queryForm
  let submission = Nothing :: Maybe Text
      handlerName = "getQueryFormR" :: Text
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "Searching for submissions"
      $(widgetFile "query-form")

postQueryFormR :: Handler Html
postQueryFormR = do
    ((result, formWidget), formEnctype) <- runFormPost queryForm
    let handlerName = "postQueryFormR" :: Text
    case result of
      FormSuccess query -> processQuery query
      _ -> defaultLayout $ do
        aDomId <- newIdent
        setTitle "Searching for submissions"
        $(widgetFile "query-form")

getQueryResultsR :: Text -> Handler Html
getQueryResultsR = processQuery

processQuery :: Text -> Handler Html
processQuery query = do
  submissions <- findSubmissions query
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "query results"
    $(widgetFile "query-results")

queryResult submission = $(widgetFile "query-result")
    where commitSha1AsText = fromSHA1ToText $ submissionCommit $ fsiSubmission submission
          submitter = formatSubmitter $ fsiUser submission
          stamp = T.pack $ show $ submissionStamp $ fsiSubmission submission

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing
