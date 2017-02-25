module Handler.Query where

import Import

import Handler.Shared
import Handler.SubmissionView
import PersistSHA1

import Database.Persist.Sql
import Data.Text as T(pack)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)


findSubmissions :: Text -> Handler [FullSubmissionInfo]
findSubmissions sha1Prefix = do
  mauthId <- maybeAuth
  submissions <- runDB $ case mauthId of
    Just (Entity authId _) -> rawSql "SELECT ?? FROM submission WHERE (is_public OR submitter = ?) AND cast(commit as text) like ?" [toPersistValue authId, PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
    Nothing -> rawSql "SELECT ?? FROM submission WHERE is_public AND cast(commit as text) like ?" [PersistText $ "\\\\x" ++ sha1Prefix ++ "%"]
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
    setTitle "query results"
    $(widgetFile "query-results")

queryForm :: Form Text
queryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgGitCommitSha1) Nothing
