module Handler.AccountReset where

import Import
import Handler.Shared
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Data.Time.Clock (addUTCTime)

import Handler.Common (passwordConfirmField, updatePassword, isPasswordAcceptable, tooWeakPasswordMessage)

data AccountStatus = NewlyCreated | PasswordReset

getCreateResetLinkR :: Handler Html
getCreateResetLinkR = do
  (formWidget, formEnctype) <- generateFormPost createResetLinkForm
  defaultLayout $ do
    setTitle "Create a reset link"
    $(widgetFile "create-reset-link")

postCreateResetLinkR :: Handler Html
postCreateResetLinkR = do
  ((result, _), _) <- runFormPost createResetLinkForm
  let mEmail = case result of
                 FormSuccess email -> Just email
                 _ -> Nothing
  doCreateResetLink mEmail

doCreateResetLink :: Maybe Text -> Handler Html
doCreateResetLink (Just email) = do
  mUserEnt <- runDB $ getBy $ UniqueUser email
  userId <- createOrUse mUserEnt email

  key <- newToken
  theNow <- liftIO getCurrentTime
  let expirationMoment = addUTCTime (60*60*24) theNow
  runDB $ update userId [UserVerificationKey =. Just key, UserKeyExpirationDate =. Just expirationMoment]

  defaultLayout $ do
     setTitle "Creating a reset link"
     $(widgetFile "reset-link-created")

doCreateResetLink Nothing = do
  setMessage $ toHtml ("No e-mail given" :: Text)
  getCreateResetLinkR

createOrUse :: Maybe (Entity User) -> Text -> Handler UserId
createOrUse (Just userEnt) _ = return $ entityKey userEnt
createOrUse Nothing email = do
  setMessage $ toHtml ("Created new user " ++ email)
  triggerToken <- newToken
  userId <- runDB $ insert $ User email Nothing Nothing False Nothing True Nothing Nothing Nothing (Just triggerToken)
  return userId

createResetLinkForm :: Form Text
createResetLinkForm = renderBootstrap3 BootstrapBasicForm
    $ areq textField (bfs MsgEMail) Nothing

getResetPasswordR :: Text -> Handler Html
getResetPasswordR key = do
  mUserId <- checkVerificationKey key
  accountStatus <- case mUserId of
    (Just userId) -> do
      user <- runDB$ get404 userId
      return $ if isJust (userPassword user)
               then
                 PasswordReset
               else
                 NewlyCreated
    _ -> return PasswordReset
  (formWidget, formEnctype) <- generateFormPost $ changePasswordForm accountStatus
  master <- getYesod
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "reset-password")

postResetPasswordR :: Text -> Handler Html
postResetPasswordR key = do
  ((result, _), _) <- runFormPost $ changePasswordForm PasswordReset
  mUserId <- checkVerificationKey key
  let mPassword = case result of
                    FormSuccess password -> Just password
                    _ -> Nothing
  doResetPassword key mUserId mPassword

doResetPassword :: Text -> Maybe (Key User) -> Maybe Text -> Handler Html
doResetPassword key _ Nothing = do
  setMessage $ toHtml ("Password not given or does not match! Make sure you entered the same password" :: Text)
  getResetPasswordR key

doResetPassword key (Just userId) (Just password) = do
  doResetPassword' (isPasswordAcceptable password) key userId password

doResetPassword key Nothing _ = do
  runDB $ updateWhere [UserVerificationKey ==. Just key, UserIsAdmin ==. False] removeVerificationKeyStatement
  master <- getYesod
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "password-reset-failed")

doResetPassword' :: Bool -> Text -> Key User -> Text -> Handler Html
doResetPassword' True _ userId password = do
  updatePassword userId (Just password)
  runDB $ update userId removeVerificationKeyStatement
  setMessage $ toHtml ("Password set! Now, you can log in with your e-mail address." :: Text)
  redirect HomeR

doResetPassword' False key _ _ = do
  tooWeakPasswordMessage
  getResetPasswordR key

removeVerificationKeyStatement :: [Update User]
removeVerificationKeyStatement = [UserVerificationKey =. Nothing,                                                         UserKeyExpirationDate =. Nothing]


checkVerificationKey :: Text -> Handler (Maybe UserId)
checkVerificationKey key = do
  theNow <- liftIO getCurrentTime
  userEnts <- runDB $ selectList [UserVerificationKey ==. Just key, UserKeyExpirationDate >. Just theNow] []
  return $ case userEnts of
                 [Entity k _] -> Just k
                 _ -> Nothing

changePasswordForm :: AccountStatus -> Form Text
changePasswordForm accountStatus = renderBootstrap3 BootstrapBasicForm
    $ areq passwordConfirmField (bfs $ passwordFormHeader accountStatus) Nothing

passwordFormHeader NewlyCreated = MsgPasswordForNewAccount
passwordFormHeader PasswordReset = MsgPassword
