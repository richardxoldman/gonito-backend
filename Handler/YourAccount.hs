module Handler.YourAccount where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bfs)

import Handler.Shared
import Handler.Extract

import Text.Regex.TDFA

getYourAccountR :: Handler Html
getYourAccountR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId
    keyS <- runDB $ selectFirst [PublicKeyUser ==. userId] []
    let key = publicKeyPubkey <$> entityVal <$> keyS
    (formWidget, formEnctype) <- generateFormPost (yourAccountForm (userName user) (userLocalId user) key)
    let submission = Nothing :: Maybe (Import.FileInfo, Text)
        handlerName = "getYourAccountR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Your account"
        $(widgetFile "your-account")

postYourAccountR :: Handler Html
postYourAccountR = do
    ((result, formWidget), formEnctype) <- runFormPost (yourAccountForm Nothing Nothing Nothing)
    let handlerName = "postYourAccountR" :: Text
        accountData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (name, localId, sshPubKey) = accountData
    userId <- requireAuthId
    updateUserAccount userId name localId sshPubKey
    user <- runDB $ get404 userId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Your account"
        $(widgetFile "your-account")

yourAccountForm :: Maybe Text -> Maybe Text -> Maybe Text -> Form (Maybe Text, Maybe Text, Maybe Text)
yourAccountForm maybeName maybeLocalId maybeSshPubKey = renderBootstrap3 BootstrapBasicForm $ (,,)
    <$> aopt textField (bfs MsgAccountName) (Just maybeName)
    <*> aopt textField (bfs MsgId) (Just maybeLocalId)
    <*> aopt textField (bfs MsgSshPubKey) (Just maybeSshPubKey)

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

updateUserAccount :: Key User -> Maybe Text -> Maybe Text -> Maybe Text -> Handler ()
updateUserAccount userId name maybeLocalId maybeSshPubKey = do
  updateJustName userId name
  updateLocalIdAndPubKey userId maybeLocalId maybeSshPubKey

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
             Nothing -> runDB $ update userId [UserLocalId =. Just localId]
          runDB $ deleteWhere [PublicKeyUser ==. userId]
          case maybeSshPubKey of
            Just key -> do
              runDB $ insert $ PublicKey {
                publicKeyUser=userId,
                publicKeyPubkey=key }
              return ()
            Nothing -> return ()
    else
      setMessage $ toHtml ("unexpected ID (use only lower-case letters, digits and hyphens, start with a letter)" :: Text)

updateLocalIdAndPubKey _ Nothing (Just _) = do
  setMessage $ toHtml ("SSH public key cannot be added without an ID" :: Text)

updateLocalIdAndPubKey _ Nothing Nothing = return ()

updateJustName :: Key User -> Maybe Text -> Handler ()
updateJustName userId name = runDB $ update userId [UserName =. name]
