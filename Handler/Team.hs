module Handler.Team where

import Import

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.Shared (fieldWithTooltip)

import PersistTeamActionType

import Data.Conduit.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

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

doMyTeams :: Handler Html
doMyTeams = do
  (formWidget, formEnctype) <- generateFormPost createTeamForm
  defaultLayout $ do
    setTitle "Teams"
    $(widgetFile "my-teams")

createTeamForm :: Form TeamCreationData
createTeamForm = renderBootstrap3 BootstrapBasicForm $ TeamCreationData
    <$> areq textField (fieldWithTooltip MsgTeamIdent MsgTeamIdentTooltip) Nothing
    <*> fileAFormOpt (bfs MsgAvatar)
