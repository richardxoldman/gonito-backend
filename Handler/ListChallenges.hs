{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Handler.ListChallenges where

import Import hiding (get, fromList, Proxy)

import Data.HashMap.Strict.InsOrd (fromList)

import Data.Proxy
import Control.Lens hiding ((.=))
import Data.Swagger
import Data.Swagger.Declare

mainCondition :: [Filter Challenge]
mainCondition = [ChallengeArchived !=. Just True]

getListChallengesR :: Handler Html
getListChallengesR = generalListChallenges mainCondition

declareListChallengesSwagger :: Declare (Definitions Schema) Swagger
declareListChallengesSwagger = do
  -- param schemas
  listChallengesResponse      <- declareResponse (Proxy :: Proxy [Entity Challenge])

  return $ mempty
    & paths .~
        [ ("/api/list-challenges", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns the list of all challenges"
            & at 200 ?~ Inline listChallengesResponse))
        ]

listChallengesApi :: Swagger
listChallengesApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareListChallengesSwagger mempty

getListChallengesJsonR :: Handler Value
getListChallengesJsonR = generalListChallengesJson mainCondition

getListArchivedChallengesR :: Handler Html
getListArchivedChallengesR = generalListChallenges [ChallengeArchived ==. Just True]

imageUrl :: Entity Challenge -> Maybe (Route App)
imageUrl (Entity challengeId challenge) =
  case challengeImage challenge of
    Just _ -> Just $ ChallengeImageR challengeId
    Nothing -> Nothing

instance ToJSON (Entity Challenge) where
    toJSON chEnt@(Entity _ ch) = object
        [ "name"  .= challengeName ch
        , "title" .= challengeTitle ch
        , "description" .= challengeDescription ch
        , "starred" .= challengeStarred ch
        , "archived" .= challengeArchived ch
        , "imageUrl" .= (("/" <>) <$> intercalate "/" <$> fst <$> renderRoute <$> imageUrl chEnt)
        ]

instance ToSchema (Entity Challenge) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    booleanSchema <- declareSchemaRef (Proxy :: Proxy Bool)
    return $ NamedSchema (Just "Challenge") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                    , ("title", stringSchema)
                    , ("description", stringSchema)
                    , ("starred", booleanSchema)
                    , ("archived", booleanSchema)
                    , ("imageUrl", stringSchema)
                    ]
        & required .~ [ "name", "title", "description", "starred", "archived" ]


generalListChallengesJson :: [Filter Challenge] -> Handler Value
generalListChallengesJson filterExpr = do
  challenges <- getChallenges filterExpr
  return $ toJSON challenges

generalListChallenges :: [Filter Challenge] -> Handler Html
generalListChallenges filterExpr = do
  challenges <- getChallenges filterExpr
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

getChallenges :: [Filter Challenge] -> Handler [Entity Challenge]
getChallenges filterExpr = runDB $ selectList filterExpr [Desc ChallengeStarred, Desc ChallengeStamp]

listChallengesCore :: [Entity Challenge] -> Widget
listChallengesCore challenges = $(widgetFile "list-challenges-core")

getChallengeImageR :: ChallengeId -> Handler Html
getChallengeImageR challengeId = do
   challenge <- runDB $ get404 challengeId
   case challengeImage challenge of
     Just image -> do
       addHeader "Content-Disposition" "attachment; filename=\"image.png\""
       sendResponse (typePng, toContent image)
     Nothing -> sendResponseStatus status202 ()
