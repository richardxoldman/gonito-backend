module Handler.SubmissionPreview where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import           Handler.MakePublic

import           Data.List (intersect)
import           Data.Text                  (splitOn, unpack)
import           Database.Persist.Sql       hiding (insert)
import           Prelude                    (read)


getSubmissionPreviewR :: SubmissionId -> Handler Html
getSubmissionPreviewR submissionId = do
    isOwner <- checkWhetherUserRepo submissionId

    if isOwner
        then do
            pure "Submission changed"
        else
            pure "Only owner can edit a submission!"


submissionPreviewApi :: Swagger
submissionPreviewApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareSubmissionPreviewApi mempty


declareSubmissionPreviewApi :: Declare (Definitions Schema) Swagger
declareSubmissionPreviewApi = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
        txtSchema = toParamSchema (Proxy :: Proxy Text)
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/submission-preview/{submissionId}", mempty
            & DS.get ?~ (mempty
                & parameters .~ 
                [
                    Inline $ mempty
                    & name .~ "submissionId"
                    & required ?~ True
                    & description ?~ "Integer, e.g.: 123"
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ idSchema
                        )
                ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Get information about a submission."
                & at 200 ?~ Inline response
                )
            )
        ]
