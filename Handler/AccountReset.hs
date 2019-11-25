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
  let mUserIdentifier = case result of
                          FormSuccess (userIdentifier, _) -> Just userIdentifier
                          _ -> Nothing
  let mCourseId = case result of
                 FormSuccess (_, Just courseId) -> Just courseId
                 _ -> Nothing
  doCreateResetLink mUserIdentifier mCourseId

doCreateResetLink :: Maybe Text -> Maybe CourseId -> Handler Html
doCreateResetLink (Just userIdentifier) mCourseId  = do
  mUserEnt <- runDB $ getBy $ UniqueUser userIdentifier
  userId <- createOrUse mUserEnt userIdentifier

  addParticipant userId mCourseId

  key <- newToken
  theNow <- liftIO getCurrentTime
  let expirationMoment = addUTCTime (60*60*24) theNow
  runDB $ update userId [UserVerificationKey =. Just key, UserKeyExpirationDate =. Just expirationMoment]

  defaultLayout $ do
     setTitle "Creating a reset link"
     $(widgetFile "reset-link-created")

doCreateResetLink Nothing _ = do
  setMessage $ toHtml ("No user identifier given" :: Text)
  getCreateResetLinkR


addParticipant :: (PersistStoreWrite (YesodPersistBackend site),
                  YesodPersist site,
                  BaseBackend (YesodPersistBackend site) ~ SqlBackend)
                 => Key User -> Maybe (Key Course) -> HandlerFor site ()
addParticipant _ Nothing = return ()
addParticipant userId (Just courseId) = do
  _ <- runDB $ insert $ Participant userId courseId
  return ()

createOrUse :: Maybe (Entity User) -> Text -> Handler UserId
createOrUse (Just userEnt) _ = return $ entityKey userEnt
createOrUse Nothing userIdentifier = do
  setMessage $ toHtml ("Created new user " ++ userIdentifier)
  triggerToken <- newToken
  userId <- runDB $ insert $ User userIdentifier
                                 Nothing
                                 Nothing
                                 False
                                 Nothing
                                 True
                                 Nothing
                                 Nothing
                                 Nothing
                                 (Just triggerToken)
  return userId

createResetLinkForm :: Form (Text, Maybe CourseId)
createResetLinkForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgUserIdentifier) Nothing
    <*> coursesSelectFieldList

coursesSelectFieldList :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, RenderMessage site FormMessage, RenderMessage site AppMessage, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => AForm (HandlerFor site) (Maybe (Key Course))
coursesSelectFieldList = aopt (selectField courses) (bfs MsgCourseOptional) Nothing
    where
      courses = do
        courseEnts <- runDB $ selectList [] [Asc CourseName]
        optionsPairs $ Import.map (\ch -> (courseName $ entityVal ch, entityKey ch)) courseEnts


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
  setMessage $ toHtml ("Password set! Now, you can log in with your login." :: Text)
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

passwordFormHeader :: AccountStatus -> AppMessage
passwordFormHeader NewlyCreated = MsgPasswordForNewAccount
passwordFormHeader PasswordReset = MsgPassword
