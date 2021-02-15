module Handler.Swagger where

import Import

import Data.Swagger
import Handler.ListChallenges
import Handler.ShowChallenge
import Handler.Query

import Control.Lens hiding ((.=))

getSwaggerR :: Handler Value
getSwaggerR = return $ toJSON apiDescription

apiDescription :: Swagger
apiDescription = generalApi
                 <> listChallengesApi
                 <> leaderboardApi
                 <> allSubmissionsApi
                 <> mySubmissionsApi
                 <> challengeReadmeInMarkdownApi
                 <> queryApi
                 <> challengeSubmissionApi

generalApi :: Swagger
generalApi = (mempty :: Swagger)
  & info .~ (mempty &
              title .~ "Gonito API" &
              version .~ "1.0.0")
