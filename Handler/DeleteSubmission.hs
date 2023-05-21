{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}

module Handler.DeleteSubmission where

import           Import               hiding (Proxy, encodeUtf8, fromList)

import           Handler.MakePublic

import           Yesod.Form.Bootstrap3      (BootstrapFormLayout (..), bfs,
                                             renderBootstrap3)

import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit.Binary

import           System.Directory
import           System.IO
import           System.Process

import           Handler.Common             (isPasswordAcceptable,
                                             passwordConfirmField,
                                             tooWeakPasswordMessage,
                                             updatePassword)
import           Handler.JWT
import           Handler.Shared

import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS

import           Control.Lens               hiding ((.=), (^.))
import           Data.Proxy                 as DPR
import           Data.Swagger.Declare

import           Data.Aeson                 hiding (Key)
import           Data.Aeson.Key             (fromText)
import           Data.Aeson.KeyMap          hiding (delete, filter, foldr,
                                             fromList, insert, map, null)

import           Data.HashMap.Strict.InsOrd (fromList)

import           Data.Text                  (strip)
import           Data.Text.Encoding         (encodeUtf8)

postDeleteSubmissionR :: SubmissionId -> Handler ()
postDeleteSubmissionR submissionId = do
    isOwner <- checkWhetherUserRepo submissionId

    if isOwner then do
            runDB $ update submissionId [SubmissionDeleted =. True]
            setMessage $ toHtml ("Submission deleted" :: Text)
        
        else
            setMessage $ toHtml ("Only owner can delete a submission!" :: Text)

declareDeleteSubmissionApi :: Declare (Definitions Schema) Swagger
declareDeleteSubmissionApi = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
    response <- declareResponse (Proxy :: Proxy String)

    pure $ mempty
        & paths .~ fromList [
            ("/api/delete-submission/{submissionId}"
            , mempty & DS.post ?~
                ( mempty
                & parameters .~ [
                    Inline $ mempty
                        & name .~ "submissionId"
                        & required ?~ True
                        & schema .~ ParamOther (mempty
                            & in_ .~ ParamPath
                            & paramSchema .~ idSchema)
                    ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Deletes the submission by setting 'true' value in the deleted field."
                & at 200 ?~ Inline response ))
            ]
