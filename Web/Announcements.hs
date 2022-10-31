{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Announcements
  (sendAnnouncement,
   formatLink,
   AnnouncementHook(..),
   AnnouncementPiece(..),
   AnnouncementMessage,
   renderAnnouncementMessage,
   toAnnouncementHook)
  where

import Data.Text
import qualified Data.Text.Encoding as DTE
import Data.Maybe
import Network.HTTP.Req as R
import Prelude
import Data.Aeson
import Data.Aeson.Key
import Text.URI as URI

data AnnouncementHook = SlackWebHook Text | DiscordWebHook Text

data AnnouncementPiece = AnnouncementText Text | AnnouncementLink Text Text

type AnnouncementMessage = [AnnouncementPiece]

renderAnnouncementMessage :: Maybe AnnouncementHook -> AnnouncementMessage -> Text
renderAnnouncementMessage hook pieces = Data.Text.concat $ Prelude.map (renderAnnouncementPiece hook) pieces

renderAnnouncementPiece :: Maybe AnnouncementHook -> AnnouncementPiece -> Text
renderAnnouncementPiece _ (AnnouncementText t) = t
renderAnnouncementPiece mHook (AnnouncementLink url title) = formatLink mHook url title

toAnnouncementHook :: Text -> AnnouncementHook
toAnnouncementHook url
  | ".slack." `isInfixOf` url = SlackWebHook url
  | "discord.com" `isInfixOf` url = DiscordWebHook url
  | otherwise = error $ unpack $ "unknown hook type '" <> url <> "'"

sendAnnouncement :: AnnouncementHook -> AnnouncementMessage -> IO ()
sendAnnouncement hook message = sendAnnouncement' hook $ renderAnnouncementMessage (Just hook) message

sendAnnouncement' :: AnnouncementHook -> Text -> IO ()
sendAnnouncement' (SlackWebHook hook) message = sendAnnouncementViaJson hook "text" message
sendAnnouncement' (DiscordWebHook hook) message = sendAnnouncementViaJson hook "content" message

sendAnnouncementViaJson :: Text -> Text -> Text -> IO ()
sendAnnouncementViaJson hook fieldName message = do
  uri <- URI.mkURI hook

  let (Just (hookUrl, _)) = useHttpsURI uri

  R.runReq defaultHttpConfig $ do
    let payload = object [ fromText fieldName .= message ]
    (_ :: IgnoreResponse) <- R.req R.POST
                                 hookUrl
                                 (R.ReqBodyJson payload)
                                 R.ignoreResponse
                                 mempty
    return ()

formatLink :: Maybe AnnouncementHook -> Text -> Text -> Text
formatLink (Just (SlackWebHook _)) url title = "<" <> url <> "|" <> title <> ">"
formatLink (Just (DiscordWebHook _)) url title = formatLinkWithAngleBrackets url title
formatLink Nothing url title = formatLinkWithAngleBrackets url title

formatLinkWithAngleBrackets :: Text -> Text -> Text
formatLinkWithAngleBrackets url title = title <> " <" <> url <> ">"
