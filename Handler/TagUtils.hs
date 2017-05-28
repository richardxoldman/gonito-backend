module Handler.TagUtils where

import Import
import Yesod.Form.Bootstrap3 (bfs)

import Data.Text as T

getAvailableTagsAsJSON = do
  tagsAvailable <- selectList [] [Asc TagName]
  return $ toJSON $ Import.map (tagName . entityVal) tagsAvailable

tagsfs :: RenderMessage site msg => msg -> FieldSettings site
tagsfs msg = attrs { fsAttrs = ("data-role"::Text,"tagsinput"::Text):(fsAttrs attrs)}
   where attrs = bfs msg


tagsAsTextToTagIds mTagsAsText = do
  let newTags = case mTagsAsText of
                       Just tags' -> Import.map T.strip $ T.split (== ',') tags'
                       Nothing -> []
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
