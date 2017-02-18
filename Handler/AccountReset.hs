module Handler.AccountReset where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Crypto.Nonce as Nonce
import System.IO.Unsafe (unsafePerformIO)

import Data.Time.Clock (addUTCTime)

import Handler.Common (passwordConfirmField, updatePassword, isPasswordAcceptable, tooWeakPasswordMessage)

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

  key <- newVerifyKey
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
  userId <- runDB $ insert $ User email Nothing Nothing False Nothing True Nothing Nothing Nothing
  return userId

createResetLinkForm :: Form Text
createResetLinkForm = renderBootstrap3 BootstrapBasicForm
    $ areq textField (bfs MsgEMail) Nothing


nonceGen :: Nonce.Generator
nonceGen = unsafePerformIO Nonce.new
{-# NOINLINE nonceGen #-}

-- | Randomly create a new verification key.
newVerifyKey :: MonadIO m => m Text
newVerifyKey = Nonce.nonce128urlT nonceGen


getResetPasswordR :: Text -> Handler Html
getResetPasswordR key = do
  (formWidget, formEnctype) <- generateFormPost changePasswordForm
  mUserId <- checkVerificationKey key
  master <- getYesod
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "reset-password")

postResetPasswordR :: Text -> Handler Html
postResetPasswordR key = do
  ((result, _), _) <- runFormPost changePasswordForm
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
  runDB $ updateWhere [UserVerificationKey ==. Just key] removeVerificationKeyStatement
  master <- getYesod
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "password-reset-failed")

doResetPassword' :: Bool -> Text -> Key User -> Text -> Handler Html
doResetPassword' True _ userId password = do
  updatePassword userId (Just password)
  runDB $ update userId removeVerificationKeyStatement
  defaultLayout $ do
    setTitle "Reset password"
    $(widgetFile "password-reset")

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

changePasswordForm :: Form Text
changePasswordForm = renderBootstrap3 BootstrapBasicForm
    $ areq passwordConfirmField (bfs MsgPassword) Nothing
