module Handler.Discussion where

import Import

import Handler.Shared

import Text.Blaze
import Text.Blaze.Html4.Strict (i)

import Handler.ShowChallenge

import Yesod.Form.Bootstrap3
import Yesod.RssFeed

import Data.Text as T (pack)
import Database.Persist.Sql

data TimelineItem = TimelineItem Text UTCTime (Entity User) Markup

getTime :: TimelineItem -> UTCTime
getTime (TimelineItem _ stamp _ _) = stamp

class ToTimelineItem a where
  getTimelineItemId :: a -> Text

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
    let itemIdentifier = getTimelineItemId sItem
    let when' = timelineWhen sItem
    who <- timelineWho sItem
    what <- timelineWhat sItem
    return $ TimelineItem itemIdentifier when' who what

instance ToTimelineItem (Entity Comment) where
  getTimelineItemId (Entity commentId _) = "comment-" ++ (T.pack $ show $ fromSqlKey $ commentId)
  timelineWhoId (Entity _ comment) = commentAuthor comment
  timelineWhen  (Entity _ comment) = commentPosted comment
  timelineWhat  (Entity _ comment) = return $ toMarkup $ commentText comment

instance ToTimelineItem (Entity Submission) where
  getTimelineItemId (Entity commentId _) = "submission-" ++ (T.pack $ show $ fromSqlKey $ commentId)
  timelineWhoId (Entity _ submission) = submissionSubmitter submission
  timelineWhen  (Entity _ submission) = submissionStamp submission
  timelineWhat  (Entity _ submission) = return $ i $ toMarkup (
    "submitted a solution:" ++ submissionDescription submission )


getChallengeDiscussionR :: Text -> Handler Html
getChallengeDiscussionR name = do
    challengeEnt@(Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    maybeUser <- maybeAuth
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
    sortedTimelineItems <- getTimelineItems challengeId
    challengeLayout True challengeEnt (discussionWidget maybeUser formWidget formEnctype name sortedTimelineItems)

getTimelineItems :: ChallengeId -> Handler [TimelineItem]
getTimelineItems challengeId = do
    comments <- runDB $ selectList [CommentChallenge ==. challengeId] [Desc CommentPosted]
    submissions <-  runDB $ selectList [SubmissionChallenge ==. challengeId] [Desc SubmissionStamp]
    timelineItems' <-  mapM toTimelineItem comments
    timelineItems'' <- mapM toTimelineItem submissions
    return $ sortBy (\item1 item2 -> (getTime item2 `compare` getTime item1)) (
      timelineItems' ++ timelineItems'')

discussionWidget maybeUser formWidget formEnctype name sortedTimelineItems = $(widgetFile "challenge-discussion")

timelineItemWidget :: TimelineItem -> WidgetFor App ()
timelineItemWidget item = $(widgetFile "timeline-item")

postChallengeDiscussionR :: Text -> Handler TypedContent
postChallengeDiscussionR name = do
    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    ((result, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (commentForm challengeId)
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


numberOfItemsInFeed :: Int
numberOfItemsInFeed = 20


getChallengeDiscussionFeedR :: Text -> Handler RepRss
getChallengeDiscussionFeedR name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  items <- getTimelineItems challengeId
  now <- liftIO getCurrentTime
  render <- getUrlRender
  rssFeedText Feed {
    feedTitle = "gonito.net / " ++ (challengeTitle challenge),
    feedLinkSelf = render HomeR,
    feedLinkHome = render (ChallengeDiscussionFeedR name),
    feedAuthor = "gonito.net",
    feedDescription = toMarkup $ "Comments and submission for a gonito.net challenge — " ++ (challengeTitle challenge),
    feedLanguage = "en",
    feedUpdated = case items of
                   (latestItem : _) -> getTime latestItem
                   _ -> now,
    feedLogo = Nothing,
    feedEntries = map (getFeedEntry render challenge) (take numberOfItemsInFeed items) }


getFeedEntry :: (Route App -> Text) -> Challenge -> TimelineItem -> FeedEntry Text
getFeedEntry render challenge (TimelineItem identifier stamp (Entity _ user) theContents) = FeedEntry {
  feedEntryLink = (render (ChallengeDiscussionR (challengeName challenge))) <> "#" <> identifier,
  feedEntryUpdated = stamp,
  feedEntryTitle = (challengeTitle challenge) ++ " / " ++ (formatSubmitter user),
  feedEntryContent = theContents,
  feedEntryEnclosure = Nothing }
