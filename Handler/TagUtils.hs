module Handler.TagUtils where

import Import
import Yesod.Form.Bootstrap3 (bfs)

import qualified Data.Set as S

import Gonito.ExtractMetadata (parseTags)

getAvailableTagsAsJSON :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) => ReaderT backend m Value
getAvailableTagsAsJSON = do
  tagsAvailable <- selectList [] [Asc TagName]
  return $ toJSON $ Import.map (tagName . entityVal) tagsAvailable

tagsfs :: RenderMessage site msg => msg -> FieldSettings site
tagsfs msg = attrs { fsAttrs = ("data-role"::Text,"tagsinput"::Text):(fsAttrs attrs)}
   where attrs = bfs msg

addTags :: (BaseBackend backend ~ SqlBackend, Element mono ~ Key Tag, Eq (Element mono), MonoFoldable mono, PersistQueryWrite backend, MonadIO m, PersistUniqueRead backend) => Key Submission -> S.Set Text -> mono -> ReaderT backend m ()
addTags submissionId tags existingOnes = do
  tids <- tagsAsTextToTagIds tags

  deleteWhere [SubmissionTagSubmission ==. submissionId, SubmissionTagTag /<-. tids]

  _ <- mapM (\tid -> insert $ SubmissionTag submissionId tid Nothing) (Import.filter (not . (`elem` existingOnes)) tids)
  return ()

tagsAsTextToTagIds :: (BaseBackend backend ~ SqlBackend, PersistUniqueRead backend, MonadIO m) => S.Set Text -> ReaderT backend m [Key Tag]
tagsAsTextToTagIds tags = do
  let newTags = S.toList $ tags
  mTs <- mapM (\t -> getBy $ UniqueTagName t) newTags
  return $ Import.map entityKey $ Import.catMaybes mTs

fragmentWithTags t tagEnts = [whamlet|
#{t}

$forall (Entity _ v) <- tagEnts
  \ <span class="label label-primary">#{tagName v}</span>
|]

fragmentWithSubmissionTags t tagEnts = [whamlet|
#{t}

$forall ((Entity _ v), (Entity sid s)) <- tagEnts
  \ <span class="label #{tagClass $ submissionTagAccepted s}" onclick="t=$(this); $.get('/toggle-submission-tag/#{toPathPiece sid}', function(data){ if (!(data == 'BLOCKED')) {t.removeClass('#{allTagClasses}'); t.addClass(data);} }); ">#{tagName v}</span>
|]

allTagClasses :: Text
allTagClasses = (tagClass $ Just True) <> " " <> (tagClass $ Just False) <> " " <> (tagClass $ Nothing);


tagClass :: Maybe Bool -> Text
tagClass (Just True) = "label-success"
tagClass (Just False) = "label-default"
tagClass Nothing = "label-primary"

toggleTag :: Maybe Bool -> Maybe Bool
toggleTag (Just True) = Just False
toggleTag _ = Just True
