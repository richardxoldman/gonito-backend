module Handler.Swagger where

import Import

import Data.Swagger
import Handler.ListChallenges
import Handler.ShowChallenge
import Handler.Query
import Handler.Tags
import Handler.Team


import Control.Lens hiding ((.=))

getSwaggerR :: Handler Value
getSwaggerR = return $ toJSON apiDescription

apiDescription :: Swagger
apiDescription = generalApi
                 <> addUserApi
                 <> userInfoApi
                 <> listChallengesApi
                 <> leaderboardApi
                 <> allSubmissionsApi
                 <> mySubmissionsApi
                 <> challengeReadmeInMarkdownApi
                 <> queryApi
                 <> challengeSubmissionApi
                 <> versionInfoApi
                 <> listTagsApi
                 <> myTeamsApi
                 <> challengeImgApi
                 <> challengeRepoApi
                 <> testProgressApi
                 <> viewProgressWithWebSockets
                 <> viewProgressLog


generalApi :: Swagger
generalApi = (mempty :: Swagger)
  & info .~ (mempty &
              title .~ "Gonito API" &
              version .~ "3.6.0")
