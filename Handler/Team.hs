{-# LANGUAGE OverloadedLists #-}

module Handler.Team where

import Import hiding (fromList)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.Shared (fieldWithTooltip)
import Handler.JWT

import PersistTeamActionType

import Data.Conduit.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Data.Swagger.Declare
import Data.Swagger hiding (Tag, tags)
import Data.Proxy as DPR
import Control.Lens hiding ((.=), (^.))
import Data.HashMap.Strict.InsOrd (fromList)

getMyTeamsR :: Handler Html
getMyTeamsR = do
  _ <- requireAuth
  doMyTeams

data TeamCreationData = TeamCreationData {
  teamCreationTeamIdent :: Text,
  teamCreationTeamAvatar :: Maybe FileInfo }

postCreateTeamR :: Handler Html
postCreateTeamR = do
  Entity userId _ <- requireAuth
  ((result, _), _) <- runFormPost createTeamForm
  case result of
    FormSuccess teamCreationData -> do
      runDB $ createTeam userId teamCreationData
    _ -> do
      return ()
  doMyTeams

createTeam :: (PersistStoreWrite backend, MonadUnliftIO m, BaseBackend backend ~ SqlBackend)
             => Key User -> TeamCreationData -> ReaderT backend m ()
createTeam userId teamCreationData = do
  let theIdent = teamCreationTeamIdent teamCreationData
  let theAvatar = teamCreationTeamAvatar teamCreationData

  avatarBytes <- case theAvatar of
                  Just avatarFile -> do
                     fileBytes <- runResourceT $ fileSource avatarFile $$ sinkLbs
                     return $ Just (S.pack . L.unpack $ fileBytes)
                  Nothing -> return Nothing

  newTeamId <- insert Team {
    teamIdent = theIdent,
    teamAvatar = avatarBytes }

  _ <- insert TeamMember {
    teamMemberUser = userId,
    teamMemberTeam = newTeamId,
    teamMemberIsCaptain = True
  }

  theNow <- liftIO getCurrentTime

  _ <- insert TeamLog {
     teamLogStamp = theNow,
     teamLogActionType = TeamCreation,
     teamLogAgens = userId,
     teamLogPatiens = Nothing,
     teamLogVerificationKey = Nothing
  }

  return ()

data TeamMemberView = TeamMemberView {
  teamMemberViewName :: Text,
  teamMemberViewIsCaptain :: Bool
} deriving (Eq, Show)

instance ToJSON TeamMemberView where
    toJSON v = object
        [ "name" .= (teamMemberViewName v)
        , "isCaptain" .= (teamMemberViewIsCaptain v)
        ]

instance ToSchema TeamMemberView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    return $ NamedSchema (Just "TeamMember") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("isCaptain", boolSchema)
                    ]
        & required .~ [ "name", "isCaptain" ]

data TeamView = TeamView {
  teamViewIdent :: Text,
  teamViewMembers :: [TeamMemberView]
} deriving (Eq, Show)

instance ToJSON TeamView where
    toJSON v = object
        [ "ident" .= (teamViewIdent v)
        , "members" .= (teamViewMembers v)
        ]

instance ToSchema TeamView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    membersSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TeamMemberView])
    return $ NamedSchema (Just "Team") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("ident", stringSchema)
                     , ("members", membersSchema)
                    ]
        & required .~ [ "ident", "members" ]

doMyTeams :: Handler Html
doMyTeams = do
  (formWidget, formEnctype) <- generateFormPost createTeamForm
  teams <- fetchMyTeams
  defaultLayout $ do
    setTitle "Teams"
    $(widgetFile "my-teams")

myTeamsApi :: Swagger
myTeamsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareMyTeamsSwagger mempty

declareMyTeamsSwagger :: Declare (Definitions Schema) Swagger
declareMyTeamsSwagger = do
  myTeamsResponse      <- declareResponse (DPR.Proxy :: DPR.Proxy [TeamView])

  return $ mempty
    & paths .~
        [ ("/api/my-teams", mempty & Data.Swagger.get ?~ (mempty
                                                          & produces ?~ MimeList ["application/json"]
                                                          & description ?~ "Returns the list of teams the user belongs to"
                                                          & at 200 ?~ Inline myTeamsResponse))
        ]


getMyTeamsJsonR :: Handler Value
getMyTeamsJsonR = do
  teams <- fetchMyTeams
  return $ toJSON teams


fetchMyTeams :: Handler [TeamView]
fetchMyTeams = do
  Entity userId _ <- requireAuthPossiblyByToken

  myTeams <- runDB $ E.select $ E.from $ \(team, member) -> do
              E.where_ (member ^. TeamMemberTeam E.==. team ^. TeamId
                        E.&&. member ^. TeamMemberUser E.==. E.val userId)
              E.orderBy [E.asc (team ^. TeamIdent)]
              return team

  mapM fetchTeamInfo myTeams

fetchTeamInfo :: (YesodPersist site,
                 BackendCompatible SqlBackend (YesodPersistBackend site),
                 PersistQueryRead (YesodPersistBackend site), PersistUniqueRead (YesodPersistBackend site))
                => Entity Team -> HandlerFor site TeamView
fetchTeamInfo (Entity teamId team) = do
  members <- runDB $ E.select $ E.from $ \(user, member) -> do
                    E.where_ (member ^. TeamMemberTeam E.==. E.val teamId
                              E.&&. member ^. TeamMemberUser E.==. user ^. UserId)
                    E.orderBy [E.asc (user ^. UserIdent)]
                    return (user, member)

  return $ TeamView {
    teamViewIdent = teamIdent team,
    teamViewMembers = map (\(u, m) -> TeamMemberView {
                              teamMemberViewName = userIdent $ entityVal u,
                              teamMemberViewIsCaptain = teamMemberIsCaptain $ entityVal m}) members }



createTeamForm :: Form TeamCreationData
createTeamForm = renderBootstrap3 BootstrapBasicForm $ TeamCreationData
    <$> areq textField (fieldWithTooltip MsgTeamIdent MsgTeamIdentTooltip) Nothing
    <*> fileAFormOpt (bfs MsgAvatar)
