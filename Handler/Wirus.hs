module Handler.Wirus where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare


postWirusR :: Handler Html
postWirusR = do
    --wirusData <- runDB $ getBy $ UniqueEmail (pack "wirus006@gmail.com")
    {-
    entityWirus <- runDB $ getBy404 $ UniqueEmail (pack "wirus006@gmail.com")

    let userId = emailUser $ entityVal entityWirus
    runDB $ updateWhere [SubmissionSubmitter ==. userId] [SubmissionDeleted =. True]
    case wirusData of
        Nothing -> pure "There is no wirus!"
        Just (Entity _ wirus) -> do
            let userId = emailUser wirus
            runDB $ updateWhere [SubmissionSubmitter ==. userId] [SubmissionDeleted =. True]

            -- runDB $ updateWhere [SubmissionSubmitter ==. wirus.user] []
    -- runDB $ updateWhere [SubmissionId ==. submissionId] [SubmissionDescription =. newDescription]
    -}

    pure "Wirus regained all submissions ;)"


wirusApi :: Swagger
wirusApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareWirusApi mempty


declareWirusApi :: Declare (Definitions Schema) Swagger
declareWirusApi = do
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/wirus", mempty
            & DS.post ?~ (mempty
                & parameters .~ []
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Brings back to life all wirus submissions."
                & at 200 ?~ Inline response
                )
            )
        ]
