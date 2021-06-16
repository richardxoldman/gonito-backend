{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Handler.ListChallenges where

import Import hiding (get, fromList, Proxy)

import Handler.Shared
import PersistSHA1

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
  challengeInfoResponse      <- declareResponse (Proxy :: Proxy (Entity Challenge))
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        [ ("/api/list-challenges", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns the list of all challenges"
            & at 200 ?~ Inline listChallengesResponse)),
          ("/api/challenge-info/{challengeName}",
            mempty & get ?~ (mempty
                                 & parameters .~ [ Inline $ mempty
                                                   & name .~ "challengeName"
                                                   & required ?~ True
                                                   & schema .~ ParamOther (mempty
                                                                            & in_ .~ ParamPath
                                                                            & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns metadata for a specific challenge"
                                        & at 200 ?~ Inline challengeInfoResponse))
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
imageUrl (Entity _ challenge) =
  case challengeImage challenge of
    Just _ -> Just $ ChallengeImgR $ challengeName challenge
    Nothing -> Nothing

instance ToJSON (Entity Challenge) where
    toJSON chEnt@(Entity _ ch) = object
        [ "name"  .= challengeName ch
        , "title" .= challengeTitle ch
        , "description" .= challengeDescription ch
        , "starred" .= challengeStarred ch
        , "archived" .= challengeArchived ch
        , "imageUrl" .= (("/" <>) <$> intercalate "/" <$> fst <$> renderRoute <$> imageUrl chEnt)
        , "version" .= (fromSHA1ToText $ challengeVersion ch)
        ]

instance ToSchema (Entity Challenge) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    booleanSchema <- declareSchemaRef (Proxy :: Proxy Bool)
    return $ NamedSchema (Just "Challenge") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [ ("name", stringSchema)
                    , ("title", stringSchema)
                    , ("description", stringSchema)
                    , ("starred", booleanSchema)
                    , ("archived", booleanSchema)
                    , ("imageUrl", stringSchema)
                    , ("version", stringSchema)
                    ]
        & required .~ [ "name", "title", "description", "starred", "archived", "version" ]


declareVersionInfoSwagger :: Declare (Definitions Schema) Swagger
declareVersionInfoSwagger = do
  -- param schemas
  versionInfoResponse      <- declareResponse (Proxy :: Proxy (Entity Version))
  let versionHashSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        [ ("/api/version-info/{challengeName}",
            mempty & get ?~ (mempty
                                 & parameters .~ [ Inline $ mempty
                                                   & name .~ "versionHash"
                                                   & required ?~ True
                                                   & schema .~ ParamOther (mempty
                                                                            & in_ .~ ParamPath
                                                                            & paramSchema .~ versionHashSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns information about a challenge version"
                                        & at 200 ?~ Inline versionInfoResponse))
        ]

versionInfoApi :: Swagger
versionInfoApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareVersionInfoSwagger mempty


instance ToJSON (Entity Version) where
    toJSON chEnt@(Entity _ ver) = object
        [ "deadline"  .= versionDeadline ver
        , "version" .= (formatVersion (versionMajor ver,
                                       versionMinor ver,
                                       versionPatch ver))
        , "description" .= versionDescription ver
        , "when" .= versionStamp ver
        , "commit" .= (fromSHA1ToText $ versionCommit ver)
        ]

instance ToSchema (Entity Version) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "Version") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("deadline", stringSchema)
                    , ("version", stringSchema)
                    , ("description", stringSchema)
                    , ("when", stringSchema)
                    , ("commit", stringSchema)
                    ]
        & required .~ [ "version", "description", "when", "commit" ]


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

getChallengeInfoJsonR :: Text -> Handler Value
getChallengeInfoJsonR challengeName = do
  entCh <- runDB $ getBy404 $ UniqueName challengeName
  return $ toJSON entCh

getVersionInfoJsonR :: Text -> Handler Value
getVersionInfoJsonR versionHash = do
  theVersion <- runDB $ getBy404 $ UniqueVersionByCommit $ fromTextToSHA1 versionHash
  return $ toJSON theVersion


getChallengeImgR :: Text -> Handler Html
getChallengeImgR chName = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName chName
   case challengeImage challenge of
     Just image -> do
       addHeader "Content-Disposition" "attachment; filename=\"image.png\""
       sendResponse (typePng, toContent image)
     Nothing -> sendResponseStatus status202 ()


declareChallengeImgSwagger :: Declare (Definitions Schema) Swagger
declareChallengeImgSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        [ ("/api/challenge-img/{challengeName}",
            mempty & get ?~ (mempty
                                 & parameters .~ [ Inline $ mempty
                                                   & name .~ "challengeName"
                                                   & required ?~ True
                                                   & schema .~ ParamOther (mempty
                                                                            & in_ .~ ParamPath
                                                                            & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["image/png"]
                                        & description ?~ "Return the main image for a challenge"))
        ]

challengeImgApi :: Swagger
challengeImgApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareChallengeImgSwagger mempty
