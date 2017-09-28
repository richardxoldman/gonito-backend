module Handler.YourAccount where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Text.Regex.TDFA

import Data.Conduit.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Handler.Common (passwordConfirmField, updatePassword, isPasswordAcceptable, tooWeakPasswordMessage)
import Handler.Shared

getYourAccountR :: Handler Html
getYourAccountR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId

    enableTriggerToken userId (userTriggerToken user)

    keyS <- runDB $ selectFirst [PublicKeyUser ==. userId] []
    let key = publicKeyPubkey <$> entityVal <$> keyS
    (formWidget, formEnctype) <- generateFormPost (yourAccountForm (userName user) (userLocalId user) key (userIsAnonymous user))
    defaultLayout $ do
        setTitle "Your account"
        $(widgetFile "your-account")

postYourAccountR :: Handler Html
postYourAccountR = do
    ((result, formWidget), formEnctype) <- runFormPost (yourAccountForm Nothing Nothing Nothing False)
    userId <- requireAuthId
    user <- runDB $ get404 userId

    enableTriggerToken userId (userTriggerToken user)

    let accountData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    case accountData of
        Just (name, localId, mPassword, sshPubKey, avatarFile, anonimised) -> do
          if checkPassword mPassword
            then
              updateUserAccount userId name localId mPassword sshPubKey avatarFile anonimised
            else
              tooWeakPasswordMessage
        Nothing -> do
          setMessage $ toHtml ("Something went wrong, probably the password did not match" :: Text)
    defaultLayout $ do
      setTitle "Your account"
      $(widgetFile "your-account")

checkPassword :: Maybe Text -> Bool
checkPassword Nothing = True
checkPassword (Just "") = True
checkPassword (Just passwd) = isPasswordAcceptable passwd

autocompleteOff name tooltip = setts { fsAttrs = (fsAttrs setts) ++ [("autocomplete", "nope")]}
   where setts = (bfs name) { fsTooltip = Just $ SomeMessage tooltip }

yourAccountForm :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Form (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe FileInfo, Bool)
yourAccountForm maybeName maybeLocalId maybeSshPubKey anonimised = renderBootstrap3 BootstrapBasicForm $ (,,,,,)
    <$> aopt textField (fieldWithTooltip MsgAccountName MsgAccountNameTooltip) (Just maybeName)
    <*> aopt textField (autocompleteOff MsgId MsgIdTooltip) (Just maybeLocalId)
    <*> aopt passwordConfirmField (bfs MsgPassword) Nothing
    <*> aopt textField (fieldWithTooltip MsgSshPubKey MsgSshPubKeyTooltip) (Just maybeSshPubKey)
    <*> fileAFormOpt (bfs MsgAvatar)
    <*> areq checkBoxField (bfs MsgWantToBeAnonimised) (Just anonimised)

localIdRegexp :: Regex
localIdRegexp = makeRegexOpts defaultCompOpt{newSyntax=True} defaultExecOpt ("\\`[a-z][-a-z0-9]{0,31}\\'" ::String)

unwantedLocalIds :: [Text]
unwantedLocalIds = ["git",
                    "gitolite",
                    "admin",
                    "root",
                    "filipg"]

isLocalIdAcceptable :: Text -> Bool
isLocalIdAcceptable localId =
  match localIdRegexp (unpack localId) && not (localId `elem` unwantedLocalIds)

updateUserAccount :: Key User -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe FileInfo -> Bool -> Handler ()
updateUserAccount userId name maybeLocalId maybePassword maybeSshPubKey maybeAvatarFile anonimised = do
  updateJustName userId name
  updateAvatar userId maybeAvatarFile
  updateLocalIdAndPubKey userId maybeLocalId maybeSshPubKey
  updateAnonimity userId anonimised
  case maybePassword of
    Nothing -> return ()
    Just "" -> return ()
    Just p -> updatePassword userId (Just p)

updateAvatar :: Key User -> Maybe FileInfo -> Handler ()
updateAvatar _ Nothing = return ()
updateAvatar userId (Just avatarFile) = do
  fileBytes <- runResourceT $ fileSource avatarFile $$ sinkLbs
  runDB $ update userId [UserAvatar =. Just (S.pack . L.unpack $ fileBytes)]

updateLocalIdAndPubKey :: Key User -> Maybe Text -> Maybe Text -> Handler ()
updateLocalIdAndPubKey userId (Just localId) maybeSshPubKey = do
  if isLocalIdAcceptable localId
    then
     do
      otherTheSame <- runDB $ selectFirst [UserLocalId ==. (Just localId), UserId !=. userId] []
      case otherTheSame of
        Just _ -> do
          setMessage $ toHtml ("ID already used" :: Text)
        Nothing -> do
          user <- runDB $ get404 userId
          case userLocalId user of
             Just prevLocalId -> do
               unless (prevLocalId == localId) $ setMessage $ toHtml ("only the administrator can change your ID" :: Text)
             Nothing -> do
               runDB $ update userId [UserLocalId =. Just localId]
               setMessage $ toHtml ("ID set" :: Text)
          runDB $ deleteWhere [PublicKeyUser ==. userId]
          case maybeSshPubKey of
            Just key -> do
              _ <- runDB $ insert $ PublicKey {
                publicKeyUser=userId,
                publicKeyPubkey=key }
              setMessage $ toHtml ("SSH public key added; now it may take 10 minutes for the keys to be active, please be patient" :: Text)
              return ()
            Nothing -> return ()
    else
      setMessage $ toHtml ("unexpected ID (use only lower-case letters, digits and hyphens, start with a letter)" :: Text)

updateLocalIdAndPubKey _ Nothing (Just _) = do
  setMessage $ toHtml ("SSH public key cannot be added without an ID" :: Text)

updateLocalIdAndPubKey _ Nothing Nothing = return ()

updateJustName :: Key User -> Maybe Text -> Handler ()
updateJustName userId name = runDB $ update userId [UserName =. name]

updateAnonimity :: Key User -> Bool -> Handler ()
updateAnonimity userId anonimised = runDB $ update userId [UserIsAnonymous =. anonimised]

getAvatarR :: UserId -> Handler TypedContent
getAvatarR userId = do
    user <- runDB $ get404 userId
    case userAvatar user of
     Just avatarBytes -> do
       addHeader "Content-Disposition" "attachment; filename=\"avatar.png\""
       sendResponse (typePng, toContent avatarBytes)
     Nothing -> do
       sendFile typeSvg "static/images/male-avatar.svg"
