module Handler.EditSubmissionV1 where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import           Handler.MakePublic

import Data.List (intersect)
import           Data.Text                  (pack, splitOn, unpack)
import           Database.Persist.Class     (insert)
import           Database.Persist.Sql       hiding (insert)
import           Prelude                    (read)


postEditSubmission1R :: SubmissionId -> Text -> Text -> Handler Html
postEditSubmission1R submissionId tagIdxsTxt newDescription = do
    isOwner <- checkWhetherUserRepo submissionId

    if isOwner
        then do
            -- Checking if tags are in the table
            tagsEntities <- runDB $ selectList [] [Asc TagId] 

            let oldTagsIds = map idFromEntity tagsEntities
                newTagIds = map mkId $ splitOn "," tagIdxsTxt
                tagsCond = Data.List.intersect oldTagsIds newTagIds == newTagIds

            if tagsCond
                then do
                    -- Change of the description
                    runDB $ updateWhere [SubmissionId ==. submissionId] [SubmissionDescription =. newDescription]

                    -- Deletion of the tags for a given submission
                    runDB $ deleteWhere [SubmissionTagSubmission ==. submissionId]

                    -- Insertion of new tags
                    let tagEntries = map (mkEntry . mkId) $ splitOn "," tagIdxsTxt
                    runDB $ insertMany_ tagEntries

                    pure "Submission tags changed"
                else
                    pure "Tags are not available in the tags table"
        else
            pure "Only owner can edit a submission!"
    where
        mkId x = toSqlKey $ fromIntegral (read (Data.Text.unpack x) :: Int)
        mkEntry x = SubmissionTag submissionId x Nothing
        idFromEntity (Entity tagId _) = tagId


editSubmission1Api :: Swagger
editSubmission1Api = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareEditSubmission1Api mempty


declareEditSubmission1Api :: Declare (Definitions Schema) Swagger
declareEditSubmission1Api = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
        txtSchema = toParamSchema (Proxy :: Proxy Text)
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/edit-submission/{submissionId}/{tagsIds}/{description}", mempty
            & DS.post ?~ (mempty
                & parameters .~ 
                [
                    Inline $ mempty
                    & name .~ "submissionId"
                    & required ?~ True
                    & description ?~ "Intiger, e.g.: 123"
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ idSchema
                        ),
                    
                    Inline $ mempty
                    & name .~ "tagsIds"
                    & description ?~ "Intigers separated with coma, e.g.: 1,2,3"
                    & required ?~ True
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ txtSchema
                        ),

                    Inline $ mempty
                    & name .~ "description"
                    & description ?~ "String, e.g.: simple description"
                    & required ?~ True
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ txtSchema
                        )
                ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Edit submission description and tag fields."
                & at 200 ?~ Inline response
                )
            )
        ]
