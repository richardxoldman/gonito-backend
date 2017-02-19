-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

import Crypto.PasswordStore
import Yesod.Auth.HashDB (defaultStrength)


-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")


passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs _ _ ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password>
            <div>confirm new password:
            <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }

updatePassword :: Key User -> Maybe Text -> Handler ()
updatePassword _ Nothing = return ()
updatePassword userId (Just password) = do
  encodedPassword <- liftIO $ makePassword (encodeUtf8 password) defaultStrength
  runDB $ update userId [UserPassword =. Just (decodeUtf8 encodedPassword)]
  setMessage $ toHtml ("Password set!" :: Text)

minPasswordLength :: Int
minPasswordLength = 10

isPasswordAcceptable :: Text -> Bool
isPasswordAcceptable p = length p >= minPasswordLength && (p /= "0123456789") && (p /= "1234567890")

tooWeakPasswordMessage :: Handler ()
tooWeakPasswordMessage =
  setMessage $ toHtml ("Password is too weak!!! A password needs to have at least " ++ (show minPasswordLength) ++ " characters.")

checkIfCanEdit :: SubmissionId -> Handler Bool
checkIfCanEdit submissionId = do
  submission <- runDB $ get404 submissionId
  mUser <- maybeAuth
  return $ case mUser of
      Just (Entity userId user) -> userId == submissionSubmitter submission || userIsAdmin user
      Nothing -> False
