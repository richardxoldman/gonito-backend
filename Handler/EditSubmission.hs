{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}

module Handler.EditSubmission where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

getEditSubmissionR :: SubmissionId -> Handler Html
getEditSubmissionR submissionId = do
  (formWidget, formEnctype) <- generateFormPost editSubmissionForm
  doEditSubmission formWidget formEnctype submissionId

postEditSubmissionR :: SubmissionId -> Handler Html
postEditSubmissionR submissionId = do
  ((result, formWidget), formEnctype) <- runFormPost editSubmissionForm
  doEditSubmission formWidget formEnctype submissionId

doEditSubmission formWidget formEnctype submissionId = do
  submission <- runDB $ get404 submissionId
  defaultLayout $ do
    setTitle "Edit a submission"
    $(widgetFile "edit-submission")

editSubmissionForm :: Form (Text, Maybe Text)
editSubmissionForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgSubmissionDescription) Nothing
    <*> aopt textField (tagsfs MsgSubmissionTags) Nothing

tagsfs :: RenderMessage site msg => msg -> FieldSettings site
tagsfs msg = attrs { fsAttrs = ("data-role"::Text,"tagsinput"::Text):(fsAttrs attrs)}
   where attrs = bfs msg
