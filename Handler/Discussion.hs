module Handler.Discussion where

import Import

import Handler.Shared

import Text.Blaze
import Text.Blaze.Html4.Strict (i)

import Handler.ShowChallenge

import Yesod.Form.Bootstrap3

data TimelineItem = TimelineItem UTCTime User Markup

getTime (TimelineItem stamp _ _) = stamp

class ToTimelineItem a where
  timelineWhen :: a -> UTCTime

  timelineWhoId :: a -> UserId
  timelineWho :: a -> Handler User
  timelineWho sItem = runDB $ get404 $ timelineWhoId sItem

  timelineWhat :: a -> Handler Markup
  toTimelineItem :: a -> Handler TimelineItem

  toTimelineItem sItem = do
    let when = timelineWhen sItem
    who <- timelineWho sItem
    what <- timelineWhat sItem
    return $ TimelineItem when who what

instance ToTimelineItem (Entity Comment) where
  timelineWhoId (Entity _ comment) = commentAuthor comment
  timelineWhen  (Entity _ comment) = commentPosted comment
  timelineWhat  (Entity _ comment) = return $ toMarkup $ commentText comment

instance ToTimelineItem (Entity Submission) where
  timelineWhoId (Entity _ submission) = submissionSubmitter submission
  timelineWhen  (Entity _ submission) = submissionStamp submission
  timelineWhat  (Entity _ submission) = return $ i $ toMarkup (
    "submitted a solution:" ++ submissionDescription submission )


getChallengeDiscussionR :: Text -> Handler Html
getChallengeDiscussionR name = do
    (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
    comments <- runDB $ selectList [CommentChallenge ==. challengeId] [Desc CommentPosted]
    submissions <-  runDB $ selectList [SubmissionChallenge ==. challengeId] [Desc SubmissionStamp]
    timelineItems' <-  mapM toTimelineItem comments
    timelineItems'' <- mapM toTimelineItem submissions
    let sortedTimelineItems = sortBy (\item1 item2 -> (getTime item2 `compare` getTime item1)) (
          timelineItems' ++ timelineItems'')
    challengeLayout True challenge (discussionWidget formWidget formEnctype name sortedTimelineItems)

discussionWidget formWidget formEnctype name sortedTimelineItems = $(widgetFile "challenge-discussion")

timelineItemWidget item = $(widgetFile "timeline-item")

postChallengeDiscussionR :: Text -> Handler TypedContent
postChallengeDiscussionR name = do
    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
    case result of
      FormSuccess comment -> do
        userId <- requireAuthId

        if commentAuthor comment == userId
          then
           do
            setMessage $ toHtml ("Comment submitted" :: Text)
            _ <- runDB $ insert comment
            return ()
          else
           do
            setMessage $ toHtml ("Wrong user ID" :: Text)
            return ()
      _ -> do
        setMessage $ toHtml ("Something went wrong" :: Text)

    redirect $ ChallengeDiscussionR name

commentForm :: Key Challenge -> AForm Handler Comment
commentForm challengeId = Comment
    <$> pure challengeId
    <*> lift requireAuthId
    <*> lift (liftIO getCurrentTime)
    <*> areq textareaField (bfs MsgCommentText) Nothing
