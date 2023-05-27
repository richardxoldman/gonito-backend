{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}

module Handler.EditSubmission where

import Import
import Handler.Common (checkIfCanEdit)
import Handler.SubmissionView
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils
import Handler.MakePublic
import Handler.Query

import Gonito.ExtractMetadata (parseTags)

import Data.Text as T

import Data.Maybe (fromJust)

postAddVariantParamR :: SubmissionId -> VariantId -> Handler Html
postAddVariantParamR submissionId variantId = do
  ((result, _), _) <- runFormPost addVariantParamForm
  let FormSuccess (pName, pValue) = result
  _ <- runDB $ insert $ Parameter {
    parameterVariant = variantId,
    parameterName = pName,
    parameterValue = pValue }
  getEditSubmissionAndVariantR submissionId variantId

getEditSubmissionR :: SubmissionId -> Handler Html
getEditSubmissionR submissionId = getEditSubmissionG submissionId Nothing

getEditSubmissionAndVariantR :: SubmissionId -> VariantId -> Handler Html
getEditSubmissionAndVariantR submissionId variantId = getEditSubmissionG submissionId (Just variantId)

getEditSubmissionG :: SubmissionId -> Maybe VariantId -> Handler Html
getEditSubmissionG submissionId mVariantId = do
  submission <- runDB $ get404 submissionId
  tags <- runDB $ getTags submissionId
  let mTagsAsText = case tags of
        [] -> Nothing
        _ -> Just $ T.intercalate ", " $ Import.map (tagName . entityVal . fst) tags
  (formWidget, formEnctype) <- generateFormPost $ editSubmissionForm (submissionDescription submission) mTagsAsText
  doEditSubmission formWidget formEnctype submissionId mVariantId

postEditSubmissionR :: SubmissionId -> Handler Html
postEditSubmissionR submissionId = postEditSubmissionG submissionId Nothing

postEditSubmissionAndVariantR :: SubmissionId -> VariantId -> Handler Html
postEditSubmissionAndVariantR submissionId variantId = postEditSubmissionG submissionId (Just variantId)

postEditSubmissionG :: SubmissionId -> Maybe VariantId -> Handler Html
postEditSubmissionG submissionId mVariantId = do
  submission <- runDB $ get404 submissionId
  ((result, _), _) <- runFormPost $ editSubmissionForm (submissionDescription submission) Nothing
  let FormSuccess (description, tags) = result
  isEditable <- checkIfCanEdit submissionId
  if isEditable
    then
     runDB $ do
      update submissionId [SubmissionDescription =. description]

      sts <- selectList [SubmissionTagSubmission ==. submissionId] []
      let currentTagIds = Import.map (submissionTagTag . entityVal) sts

      addTags submissionId (parseTags tags) currentTagIds

      return ()
    else
     do
      setMessage $ toHtml ("Only owner can edit a submission!!!" :: Text)
      return ()
  getEditSubmissionG submissionId mVariantId


getPossibleAchievements :: (BaseBackend backend ~ SqlBackend, PersistUniqueRead backend, PersistQueryRead backend, MonadIO m) => Key User -> Key Submission -> ReaderT backend m [(Entity Achievement, Key WorkingOn)]
getPossibleAchievements userId submissionId = do
  submission <- get submissionId
  let challengeId = submissionChallenge (fromJust submission)
  achievements <- selectList [AchievementChallenge ==. challengeId] []
  workingOns <- mapM (\a -> getBy $ UniqueWorkingOnAchievementUser (entityKey a) userId) achievements
  let rets = Import.zip achievements workingOns
  return $ Import.map (\(a, Just w) -> (a, entityKey w)) $ Import.filter (\(_, mw) -> isJust mw) rets

doEditSubmission formWidget formEnctype submissionId mVariantId = do
  submission <- runDB $ get404 submissionId
  submissionFull <- getFullInfo (Entity submissionId submission)
  (Entity userId _) <- requireAuth
  let view = queryResult (Just userId) submissionFull

  tagsAvailableAsJSON <- runDB getAvailableTagsAsJSON

  achievements <- runDB $ getPossibleAchievements userId submissionId

  variantParams <- case mVariantId of
    Just variantId -> runDB $ selectList [ParameterVariant ==. variantId] [Asc ParameterName]
    Nothing -> return []

  (addVariantParamWidget, formEnctype2) <- generateFormPost addVariantParamForm

  defaultLayout $ do
    setTitle "Edit a submission"
    $(widgetFile "edit-submission")

editSubmissionForm :: Text -> Maybe Text -> Form (Text, Maybe Text)
editSubmissionForm description mTags = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgSubmissionDescription) (Just description)
    <*> aopt textField (tagsfs MsgSubmissionTags) (Just mTags)


addVariantParamForm :: Form (Text, Text)
addVariantParamForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgParameterName) Nothing
    <*> areq textField (bfs MsgParameterValue) Nothing


getHideSubmissionR :: SubmissionId -> Handler Html
getHideSubmissionR = changeSubmissionVisibility False

getRestoreSubmissionR :: SubmissionId -> Handler Html
getRestoreSubmissionR = changeSubmissionVisibility True


changeSubmissionVisibility :: Bool -> SubmissionId -> Handler Html
changeSubmissionVisibility status submissionId =
 do
  isOwner <- checkWhetherUserRepo submissionId
  if isOwner
    then
     do
      runDB $ update submissionId [SubmissionIsHidden =. not status]
      setMessage $ toHtml (("Submission " :: Text) ++ verb status)
    else
      setMessage $ toHtml ("Only owner can edit a submission!!!" :: Text)
  getEditSubmissionR submissionId
  where verb True = "restored"
        verb False = "removed"
