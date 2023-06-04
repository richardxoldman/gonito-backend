module Handler.EditSubmissionV1 where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import           Handler.MakePublic

postEditSubmission1R :: SubmissionId -> TagId -> Handler ()
postEditSubmission1R submissionId tagId = do
    isOwner <- checkWhetherUserRepo submissionId

    if isOwner then do
            runDB $ update submissionId [SubmissionTagTag =. tagId]
            setMessage $ toHtml ("Submission tag changed" :: Text)
        
        else
            setMessage $ toHtml ("Only owner can change a submission tags!" :: Text)

editSubmission1Api :: Swagger
editSubmission1Api = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareEditSubmission1Api mempty


declareEditSubmission1Api :: Declare (Definitions Schema) Swagger
declareEditSubmission1Api = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty
        & paths .~ fromList [
            ("/api/edit-submission/{submissionId}/{tagId}"
            , mempty & DS.post ?~
                ( mempty
                & parameters .~ 
                    [ Inline $ mempty
                        & name .~ "submissionId"
                        & required ?~ True
                        & schema .~ ParamOther (mempty
                            & in_ .~ ParamPath
                            & paramSchema .~ idSchema)
                    , Inline $ mempty
                        & name .~ "tagId"
                        & required ?~ True
                        & schema .~ ParamOther (mempty
                            & in_ .~ ParamPath
                            & paramSchema .~ idSchema)
                    ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Edit submission description and tag fields."
                & at 200 ?~ Inline response ))
            ]

