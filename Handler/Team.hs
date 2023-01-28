{-# LANGUAGE OverloadedLists #-}

module Handler.Team where

import Import hiding (fromList)

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.Shared (fieldWithTooltip)
import Handler.JWT
import Handler.AccountReset

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

import qualified Data.Text as T

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

  addMemberToTeam userId newTeamId True

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
        & type_ .~ Just SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("isCaptain", boolSchema)
                    ]
        & required .~ [ "name", "isCaptain" ]

data TeamView = TeamView {
  teamViewId :: TeamId,
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
        & type_ .~ Just SwaggerObject
        & properties .~
           fromList [  ("ident", stringSchema)
                     , ("members", membersSchema)
                    ]
        & required .~ [ "ident", "members" ]

invitation :: TeamId -> WidgetFor App ()
invitation teamId = do
  (formWidget, formEnctype) <- handlerToWidget $ generateFormPost $ teamInvitationForm $ Just teamId
  $(widgetFile "team-invitation-form")

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

  myTeams <- runDB $ E.select $ E.from $ \(team, tmember) -> do
              E.where_ (tmember ^. TeamMemberTeam E.==. team ^. TeamId
                        E.&&. tmember ^. TeamMemberUser E.==. E.val userId)
              E.orderBy [E.asc (team ^. TeamIdent)]
              return team

  mapM fetchTeamInfo myTeams

fetchTeamInfo :: (YesodPersist site,
                 BackendCompatible SqlBackend (YesodPersistBackend site),
                 PersistQueryRead (YesodPersistBackend site), PersistUniqueRead (YesodPersistBackend site))
                => Entity Team -> HandlerFor site TeamView
fetchTeamInfo (Entity teamId team) = do
  members <- runDB $ E.select $ E.from $ \(user, tmember) -> do
                    E.where_ (tmember ^. TeamMemberTeam E.==. E.val teamId
                              E.&&. tmember ^. TeamMemberUser E.==. user ^. UserId)
                    E.orderBy [E.asc (user ^. UserIdent)]
                    return (user, tmember)

  return $ TeamView {
    teamViewId = teamId,
    teamViewIdent = teamIdent team,
    teamViewMembers = map (\(u, m) -> TeamMemberView {
                              teamMemberViewName = userIdent $ entityVal u,
                              teamMemberViewIsCaptain = teamMemberIsCaptain $ entityVal m}) members }



createTeamForm :: Form TeamCreationData
createTeamForm = renderBootstrap3 BootstrapBasicForm $ TeamCreationData
    <$> areq textField (fieldWithTooltip MsgTeamIdent MsgTeamIdentTooltip) Nothing
    <*> fileAFormOpt (bfs MsgAvatar)

teamInvitationForm :: Maybe TeamId -> Form (Text, TeamId)
teamInvitationForm teamId = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField (bfs MsgInviteToTeam) Nothing
  <*> areq hiddenField "" teamId

createTeamInvitationLink :: Key User -> Text -> Key Team -> HandlerFor App (Maybe Text)
createTeamInvitationLink userId ident teamId = do
  result <- runDB $ selectList [TeamMemberUser ==. userId, TeamMemberIsCaptain ==. True, TeamMemberTeam ==. teamId] []
  case result of
    [] -> return Nothing
    _ -> do
      (key, expirationMoment) <- createLinkToken
      theNow <- liftIO getCurrentTime

      mInvitee <- runDB $ getBy $ UniqueUser ident

      case mInvitee of
        Nothing -> do
          -- we do this quietly not to leak username IDs
          return ()
        Just (Entity inviteeId _) -> do
          _ <- runDB $ insert $ TeamLog {
                teamLogStamp = theNow,
                teamLogActionType = TeamInvitation,
                teamLogAgens = userId,
                teamLogPatiens = Just inviteeId,
                teamLogTeam = Just teamId,
                teamLogVerificationKey = Just key,
                teamLogKeyExpirationDate = Just expirationMoment }
          return ()

      return $ Just key

postCreateTeamInvitationLinkR :: Handler Html
postCreateTeamInvitationLinkR = do
  Entity userId _ <- requireAuthPossiblyByToken
  ((result, _), _) <- runFormPost $ teamInvitationForm Nothing

  let FormSuccess (ident', teamId) = result

  let ident = T.strip ident'

  mToken <- createTeamInvitationLink userId ident teamId

  case mToken of
    Just token -> do
      defaultLayout $ do
        setTitle "Invitation link"
        $(widgetFile "invitation-link-created")
    Nothing -> do
      setMessage $ toHtml ("You must be a team captain to invite other people" :: Text)
      doMyTeams

checkTeamInvitationKey :: UserId -> Text -> Handler (Maybe (Entity Team, Entity User))
checkTeamInvitationKey userId key = do
  theNow <- liftIO getCurrentTime
  teamLogEntry <- runDB $ selectList [TeamLogVerificationKey ==. Just key,
                                     TeamLogPatiens ==. Just userId,
                                     TeamLogKeyExpirationDate >. Just theNow] []
  case teamLogEntry of
    [Entity _ entry] -> do
      let inviterId = teamLogAgens entry
      inviter <- runDB $ get404 inviterId
      let (Just teamId) = teamLogTeam entry
      team <- runDB $ get404 teamId
      return $ Just ((Entity teamId team), (Entity inviterId inviter))
    _ -> return Nothing

addMemberToTeam :: (MonadIO m, PersistStoreWrite backend, BaseBackend backend ~ SqlBackend)
                  => Key User -> Key Team -> Bool -> ReaderT backend m ()
addMemberToTeam userId teamId isCaptain = do
  _ <- insert TeamMember {
    teamMemberUser = userId,
    teamMemberTeam = teamId,
    teamMemberIsCaptain = isCaptain
  }

  theNow <- liftIO getCurrentTime

  _ <- insert TeamLog {
     teamLogStamp = theNow,
     teamLogActionType = TeamCreation,
     teamLogAgens = userId,
     teamLogPatiens = Nothing,
     teamLogTeam = Just teamId,
     teamLogVerificationKey = Nothing,
     teamLogKeyExpirationDate = Nothing
  }

  return ()

getTeamInvitationLinkR :: Text -> Handler Html
getTeamInvitationLinkR key = do
  Entity userId _ <- requireAuthPossiblyByToken

  result <- checkTeamInvitationKey userId key

  case result of
    Just (team, inviter) -> do
      defaultLayout $ do
        setTitle "Invitation link"
        $(widgetFile "receive-invitation-link")
    Nothing -> do
      setMessage $ toHtml ("There is something wrong with this invitation link" :: Text)
      doMyTeams

postTeamInvitationLinkR :: Text -> Handler Html
postTeamInvitationLinkR key = do
  Entity userId _ <- requireAuthPossiblyByToken

  result <- checkTeamInvitationKey userId key

  case result of
    Just (team, _) -> do
      runDB $ addMemberToTeam userId (entityKey team) False
      setMessage $ toHtml ("You joined " <> (teamIdent $ entityVal team))
    Nothing -> do
      setMessage $ toHtml ("There is something wrong with this invitation link" :: Text)

  doMyTeams

isChallengeAccessibleForUser :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Challenge -> Maybe UserId -> ReaderT backend m Bool
isChallengeAccessibleForUser challenge mUserId = do
  case challengeTeamId challenge of
    Nothing -> pure True
    Just teamId ->
      case mUserId of
        Nothing -> pure False
        Just userId ->  do
          result <- selectList [TeamMemberTeam ==. teamId, TeamMemberUser ==. userId] [LimitTo 1]
          pure $ not $ null result
