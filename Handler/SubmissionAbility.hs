{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SubmissionAbility where

import           Import                     hiding (Proxy, encodeUtf8, fromList)
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


getUserSubmissionAbilityR :: Handler Html
getUserSubmissionAbilityR = checkUserSubmissionAbility


checkUserSubmissionAbility :: Handler Html
checkUserSubmissionAbility = do
    pure "example ability"


userSubmissionAbilityApi :: Swagger
userSubmissionAbilityApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareUserSubmissionAbilityInfoApi mempty


declareUserSubmissionAbilityInfoApi :: Declare (Definitions Schema) Swagger
declareUserSubmissionAbilityInfoApi = do
    response <- declareResponse (Proxy :: Proxy String)

    pure $ mempty
        & paths .~ fromList [
            ("/api/user-submission-ability"
            , mempty & DS.get ?~
                ( mempty
                & parameters .~ [ ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Returns information if a user is able to submitt solution"
                & at 200 ?~ Inline response ))
            ]
