module Handler.DeleteSubmission where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import           Handler.MakePublic

postDeleteSubmissionR :: SubmissionId -> Handler Html
postDeleteSubmissionR submissionId = do
    isOwner <- checkWhetherUserRepo submissionId

    if isOwner
        then do
            runDB $ update submissionId [SubmissionDeleted =. True]
            pure "deleted"

        else
            pure "Only owner can delete a submission!"

deleteSubmissionApi :: Swagger
deleteSubmissionApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareDeleteSubmissionApi mempty

declareDeleteSubmissionApi :: Declare (Definitions Schema) Swagger
declareDeleteSubmissionApi = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
    response <- declareResponse (Proxy :: Proxy String)

    pure $ mempty & paths .~ fromList
        [
            ( "/api/delete-submission/{submissionId}", mempty
            & DS.post ?~ (mempty
                & parameters .~
                [
                    Inline $ mempty
                    & name .~ "submissionId"
                    & required ?~ True
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ idSchema
                        )
                ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Deletes the submission by setting 'true' value in the 'deleted' field in 'Submission' table."
                & at 200 ?~ Inline response
                )
            )
        ]
