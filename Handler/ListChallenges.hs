{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Handler.ListChallenges where

import Import hiding (get, fromList, Proxy)

import PersistSHA1

import Data.HashMap.Strict.InsOrd (fromList)

import Data.Proxy
import Control.Lens hiding ((.=))
import Data.Swagger hiding (Tag(..))
import Data.Swagger.Declare

import Data.Text (splitOn, strip)

import qualified Data.Set as S

import Handler.Tags ()

-- helper data type combining information on a challenge
-- from various tables
data ChallengeView = ChallengeView {
  challengeViewChallenge :: Entity Challenge,
  challengeViewTags :: [Entity Tag]
}

instance ToJSON ChallengeView where
    toJSON chV = object
        [ "name"  .= challengeName ch
        , "title" .= challengeTitle ch
        , "description" .= challengeDescription ch
        , "starred" .= challengeStarred ch
        , "archived" .= challengeArchived ch
        , "imageUrl" .= (("/" <>) <$> intercalate "/" <$> fst <$> renderRoute <$> imageUrl chEnt)
        , "version" .= (fromSHA1ToText $ challengeVersion ch)
        , "tags" .= challengeViewTags chV
        ]
      where ch = entityVal chEnt
            chEnt = challengeViewChallenge chV

instance ToSchema ChallengeView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    booleanSchema <- declareSchemaRef (Proxy :: Proxy Bool)
    tagsSchema <- declareSchemaRef (Proxy :: Proxy [Entity Tag])
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
                    , ("tags", tagsSchema)
                    ]
        & required .~ [ "name", "title", "description", "starred", "archived", "version" ]


mainCondition :: [Filter Challenge]
mainCondition = [ChallengeArchived !=. Just True]

getListChallengesR :: Handler Html
getListChallengesR = generalListChallenges mainCondition

declareListChallengesSwagger :: Declare (Definitions Schema) Swagger
declareListChallengesSwagger = do
  -- param schemas
  listChallengesResponse      <- declareResponse (Proxy :: Proxy [ChallengeView])
  challengeInfoResponse      <- declareResponse (Proxy :: Proxy ChallengeView)
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)
  let tagFilterSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        [ ("/api/list-challenges", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns the list of all challenges"
            & at 200 ?~ Inline listChallengesResponse)),
          ("/api/list-challenges-by-tag/{tagFilter}", mempty & get ?~ (mempty
            & parameters .~ [ Inline $ mempty
                                       & name .~ "tagFilter"
                                       & required ?~ True
                                       & description ?~ "A tag or a list of tags separated by commas"
                                       & schema .~ ParamOther (mempty
                                                               & in_ .~ ParamPath
                                                               & paramSchema .~ tagFilterSchema) ]
            & produces ?~ MimeList ["application/json"]
            & description ?~ "Returns the list of all challenges with a given combination of tags"
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

declareVersionInfoSwagger :: Declare (Definitions Schema) Swagger
declareVersionInfoSwagger = do
  -- param schemas
  versionInfoResponse      <- declareResponse (Proxy :: Proxy (Entity Version))
  let versionHashSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        [ ("/api/version-info/{versionHash}",
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
    toJSON (Entity _ ver) = object
        [ "deadline"  .= versionDeadline ver
        , "version" .= ((versionMajor ver),
                        (versionMinor ver),
                        (versionPatch ver))
        , "description" .= versionDescription ver
        , "when" .= versionStamp ver
        , "commit" .= (fromSHA1ToText $ versionCommit ver)
        ]

versionSchema :: Referenced Schema
versionSchema = Inline $ toSchema (Proxy :: Proxy [Int])
  & description .~ Just "Challenge version"
  & example .~ Just (toJSON ([2, 0, 1] :: [Int]))

instance ToSchema (Entity Version) where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (Proxy :: Proxy String)
    return $ NamedSchema (Just "Version") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [ ("deadline", stringSchema)
                    , ("version", versionSchema)
                    , ("description", stringSchema)
                    , ("when", stringSchema)
                    , ("commit", stringSchema)
                    ]
        & required .~ [ "version", "description", "when", "commit" ]


getChallengeTags :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) => ChallengeId -> ReaderT backend m [Entity Tag]
getChallengeTags challengeId = do
  sts <- selectList [ChallengeTagChallenge ==. challengeId] []
  let tagIds = Import.map (challengeTagTag . entityVal) sts
  ts <- mapM get404 $ tagIds
  let tagEnts = Import.map (\(k, v) -> Entity k v) $ Import.zip tagIds ts
  return tagEnts

fetchChallengeView :: Entity Challenge -> Handler ChallengeView
fetchChallengeView entCh@(Entity challengeId _) = do
  ts <- runDB $ getChallengeTags challengeId
  return $ ChallengeView {
     challengeViewChallenge = entCh,
     challengeViewTags = ts
  }


applyTagFilter :: Text -> ChallengeView -> Bool
applyTagFilter tagFilter chV = filterTags `S.isSubsetOf` challengeTags
  where filterTags = S.fromList $ map strip $ splitOn "," tagFilter
        challengeTags = S.fromList $ map (tagName . entityVal) $ challengeViewTags chV

getChallengesByTag :: Text -> Handler [ChallengeView]
getChallengesByTag tagFilter = do
  challenges <- getChallenges mainCondition
  return $ filter (applyTagFilter tagFilter) challenges

getListChallengesByTagR :: Text -> Handler Html
getListChallengesByTagR tagFilter = do
  challenges <- getChallengesByTag tagFilter
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

getListChallengesByTagJsonR :: Text -> Handler Value
getListChallengesByTagJsonR tagFilter = toJSON <$> getChallengesByTag tagFilter

generalListChallengesJson :: [Filter Challenge] -> Handler Value
generalListChallengesJson filterExpr = toJSON <$> getChallenges filterExpr

generalListChallenges :: [Filter Challenge] -> Handler Html
generalListChallenges filterExpr = do
  challenges <- getChallenges filterExpr
  defaultLayout $ do
    setTitle "List challenges"
    $(widgetFile "list-challenges")

getChallenges :: [Filter Challenge] -> Handler [ChallengeView]
getChallenges filterExpr = do
  challenges <- runDB $ selectList filterExpr [Desc ChallengeStarred, Desc ChallengeStamp]
  mapM fetchChallengeView challenges

listChallengesCore :: [ChallengeView] -> Widget
listChallengesCore challenges = $(widgetFile "list-challenges-core")

getChallengeInfoJsonR :: Text -> Handler Value
getChallengeInfoJsonR challengeName = do
  entCh <- runDB $ getBy404 $ UniqueName challengeName
  chV <- fetchChallengeView entCh
  return $ toJSON chV

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
