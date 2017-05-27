{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}

module Handler.EditSubmission where

import Import
import Handler.Common (checkIfCanEdit)
import Handler.SubmissionView
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import Handler.TagUtils

import Data.Text as T

getEditSubmissionR :: SubmissionId -> Handler Html
getEditSubmissionR submissionId = do
  submission <- runDB $ get404 submissionId
  tags <- runDB $ getTags submissionId
  let mTagsAsText = case tags of
        [] -> Nothing
        _ -> Just $ T.intercalate ", " $ Import.map (tagName . entityVal . fst) tags
  (formWidget, formEnctype) <- generateFormPost $ editSubmissionForm (submissionDescription submission) mTagsAsText
  doEditSubmission formWidget formEnctype submissionId

postEditSubmissionR :: SubmissionId -> Handler Html
postEditSubmissionR submissionId = do
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

      addTags submissionId tags currentTagIds

      return ()
    else
     do
      setMessage $ toHtml ("Only owner can edit a submission!!!" :: Text)
      return ()
  getEditSubmissionR submissionId


getPossibleAchievements userId submissionId = do
  (Just submission) <- get submissionId
  let challengeId = submissionChallenge submission
  achievements <- selectList [AchievementChallenge ==. challengeId] []
  workingOns <- mapM (\a -> getBy $ UniqueWorkingOnAchievementUser (entityKey a) userId) achievements
  let rets = Import.zip achievements workingOns
  return $ Import.map (\(a, (Just w)) -> (a, entityKey w)) $ Import.filter (\(_, mw) -> isJust mw) $ rets

addTags submissionId tagsAsText existingOnes = do
  tids <- tagsAsTextToTagIds tagsAsText

  deleteWhere [SubmissionTagSubmission ==. submissionId, SubmissionTagTag /<-. tids]

  _ <- mapM (\tid -> insert $ SubmissionTag submissionId tid Nothing) (Import.filter (not . (`elem` existingOnes)) tids)
  return ()



doEditSubmission formWidget formEnctype submissionId = do
  submission <- runDB $ get404 submissionId
  submissionFull <- getFullInfo (Entity submissionId submission)
  let view = queryResult submissionFull

  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON

  (Entity userId user) <- requireAuth

  achievements <- runDB $ getPossibleAchievements userId submissionId

  defaultLayout $ do
    setTitle "Edit a submission"
    $(widgetFile "edit-submission")

editSubmissionForm :: Text -> Maybe Text -> Form (Text, Maybe Text)
editSubmissionForm description mTags = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (bfs MsgSubmissionDescription) (Just description)
    <*> aopt textField (tagsfs MsgSubmissionTags) (Just mTags)
