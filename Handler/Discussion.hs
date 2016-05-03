module Handler.Discussion where

import Import

import Handler.Shared

import Text.Blaze
import Text.Blaze.Html4.Strict (i)

import Handler.ShowChallenge

import Yesod.Form.Bootstrap3

data TimelineItem = TimelineItem UTCTime (Entity User) Markup

getTime (TimelineItem stamp _ _) = stamp

class ToTimelineItem a where
  timelineWhen :: a -> UTCTime

  timelineWhoId :: a -> UserId
  timelineWho :: a -> Handler (Entity User)
  timelineWho sItem = do
    let userId = timelineWhoId sItem
    user <- runDB $ get404 userId
    return $ Entity userId user

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
    maybeUser <- maybeAuth
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
    sortedTimelineItems <- getTimelineItems challengeId
    challengeLayout True challenge (discussionWidget maybeUser formWidget formEnctype name sortedTimelineItems)

getTimelineItems :: ChallengeId -> Handler [TimelineItem]
getTimelineItems challengeId = do
    comments <- runDB $ selectList [CommentChallenge ==. challengeId] [Desc CommentPosted]
    submissions <-  runDB $ selectList [SubmissionChallenge ==. challengeId] [Desc SubmissionStamp]
    timelineItems' <-  mapM toTimelineItem comments
    timelineItems'' <- mapM toTimelineItem submissions
    return $ sortBy (\item1 item2 -> (getTime item2 `compare` getTime item1)) (
      timelineItems' ++ timelineItems'')

discussionWidget maybeUser formWidget formEnctype name sortedTimelineItems = $(widgetFile "challenge-discussion")

timelineItemWidget item = $(widgetFile "timeline-item")

postChallengeDiscussionR :: Text -> Handler TypedContent
postChallengeDiscussionR name = do
    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
    stamp <- liftIO getCurrentTime
    userId <- requireAuthId

    case result of
      FormSuccess (challengeId, commentContent) -> do
        setMessage $ toHtml ("Comment submitted" :: Text)
        _ <- runDB $ insert $ Comment challengeId userId stamp commentContent
        return ()
      _ -> do
        setMessage $ toHtml ("Something went wrong" :: Text)

    redirect $ ChallengeDiscussionR name

commentForm :: Key Challenge -> AForm Handler (ChallengeId, Textarea)
commentForm challengeId = (,)
    <$> pure challengeId
    <*> areq textareaField (bfs MsgCommentText) Nothing
