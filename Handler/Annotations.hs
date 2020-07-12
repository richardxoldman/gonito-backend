module Handler.Annotations where

import Import

import qualified Database.Esqueleto as E

import qualified Yesod.Table as Table
import Handler.Tables (timestampCell)


getListAnnotationsR :: Handler Html
getListAnnotationsR = do
  annotations <- runDB $ selectList [] [Asc AnnotationTaskId]

  defaultLayout $ do
    setTitle "List annotation tasks"
    $(widgetFile "list-annotations")



getAnnotationTaskR :: AnnotationTaskId -> Handler Html
getAnnotationTaskR annotationTaskId = do
  (Entity userId _) <- requireAuth

  annotationTask <- runDB $ get annotationTaskId

  -- Get all labels
  labels <- runDB $ selectList [AnnotationLabelAnnotationTask ==. annotationTaskId] [Asc AnnotationLabelOrder]

  -- Get list of user decisions for this annotation task
  taskDecisionIds' <- runDB $ E.select
                            $ E.from $ \(annotationItem `E.InnerJoin` annotationDecision) -> do
                              E.on (annotationItem E.^. AnnotationItemId E.==. annotationDecision E.^. AnnotationDecisionAnnotationItem)
                              E.where_ (annotationItem E.^. AnnotationItemAnnotationTask E.==. E.val annotationTaskId
                                        E.&&. annotationDecision E.^. AnnotationDecisionUser E.==. E.val userId)
                              return $ annotationDecision E.^. AnnotationDecisionAnnotationItem
  let decisionLength = length taskDecisionIds'

  -- Get number of items for this annotation task
  taskItemsLength <- runDB $ count [AnnotationItemAnnotationTask ==. annotationTaskId]

  -- Get first item to annotate which was not annotated
  let taskDecisionIds = map E.unValue taskDecisionIds'
  maybeItem <- runDB $ selectFirst [AnnotationItemAnnotationTask ==. annotationTaskId,
                                    AnnotationItemId /<-. taskDecisionIds] [Asc AnnotationItemOrder]
  let allDone = decisionLength == taskItemsLength && decisionLength > 0

  defaultLayout $ do
    setTitle "Annotation task"
    $(widgetFile "annotation-task")


postAnnotationTaskDecisionR :: AnnotationTaskId -> AnnotationItemId -> AnnotationLabelId -> Handler Html
postAnnotationTaskDecisionR annotationTaskId itemId labelId = do
  (Entity userId _) <- requireAuth
  now <- liftIO getCurrentTime
  label <- runDB $ get404 labelId

  -- Check decision exists
  maybeSaveDecision <- runDB $ selectFirst [AnnotationDecisionAnnotationItem ==. itemId,
                                            AnnotationDecisionUser ==. userId] []
  case maybeSaveDecision of
    -- Update if exists
    Nothing -> do
      _ <- runDB $ insert $ AnnotationDecision itemId userId (annotationLabelValue label) now
      return ()
    -- Insert new
    Just (Entity saveDecisionId _) -> do
      _ <- runDB $ updateWhere [AnnotationDecisionId ==. saveDecisionId] [AnnotationDecisionValue =. annotationLabelValue label,
                                                                          AnnotationDecisionStamp =. now]
      return ()

  -- Redirect to annotation page
  redirect $ AnnotationTaskR annotationTaskId


getAnnotationTaskResultsR :: AnnotationTaskId -> Handler Html
getAnnotationTaskResultsR annotationTaskId = do
  results <- runDB $ E.select
                   $ E.from $ \(annotationDecision `E.InnerJoin` annotationItem) -> do
                     E.on (annotationDecision E.^. AnnotationDecisionAnnotationItem E.==. annotationItem E.^. AnnotationItemId)
                     E.where_ (annotationItem E.^. AnnotationItemAnnotationTask E.==. E.val annotationTaskId)
                     E.orderBy [E.asc (annotationItem E.^. AnnotationItemOrder),
                                E.asc (annotationDecision E.^. AnnotationDecisionUser)]
                     return (annotationItem E.^. AnnotationItemContent,
                             annotationItem E.^. AnnotationItemOrder,
                             annotationDecision E.^. AnnotationDecisionUser,
                             annotationDecision E.^. AnnotationDecisionValue)

  labels <- runDB $ selectList [AnnotationLabelAnnotationTask ==. annotationTaskId] [Asc AnnotationLabelOrder]

  defaultLayout $ do
    setTitle "Annotation task results"
    $(widgetFile "annotation-task-results")


annotationListTable :: Table.Table App (Entity AnnotationTask)
annotationListTable = mempty
  ++ Table.text "Name" (annotationTaskName . entityVal)
  ++ Table.linked "Annotation link" (const "Annotation link") (AnnotationTaskR . entityKey)
  ++ Table.linked "Results link" (const "Results link") (AnnotationTaskResultsR . entityKey)
  ++ timestampCell "Date added" (annotationTaskAdded . entityVal)


annotationResultsTable :: Table.Table App (E.Value Text, E.Value Int, E.Value (Key User), E.Value Text)
annotationResultsTable = mempty
  ++ Table.int "ID" (\(_, order, _, _) -> E.unValue order)
  ++ Table.int "UserID" (\(_, _, userId, _) -> fromIntegral $ E.fromSqlKey $ E.unValue userId)
  ++ Table.text "Answer value" (\(_, _, _, answer) -> E.unValue answer)
  ++ Table.text "Text" (\(content, _, _, _) -> E.unValue content)


annotationLabelsTable :: Table.Table App (Entity AnnotationLabel)
annotationLabelsTable = mempty
  ++ Table.text "Answer value" (annotationLabelValue . entityVal)
  ++ Table.text "Text" (annotationLabelName . entityVal)
