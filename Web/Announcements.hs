{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Announcements
  (sendAnnouncement,
   formatLink,
   AnnouncementHook(..),
   toAnnouncementHook)
  where

import Data.Text
import qualified Data.Text.Encoding as DTE
import Data.Maybe
import Network.HTTP.Req as R
import Prelude
import Data.Aeson
import Data.Default

data AnnouncementHook = SlackWebHook Text | DiscordWebHook Text

toAnnouncementHook :: Text -> AnnouncementHook
toAnnouncementHook url
  | ".slack." `isInfixOf` url = SlackWebHook url
  | "discord.com" `isInfixOf` url = DiscordWebHook url
  | otherwise = error $ unpack $ "unknown hook type '" <> url <> "'"

sendAnnouncement :: AnnouncementHook -> Text -> IO ()
sendAnnouncement (SlackWebHook hook) message = sendAnnouncementViaJson hook "text" message
sendAnnouncement (DiscordWebHook hook) message = sendAnnouncementViaJson hook "content" message

sendAnnouncementViaJson :: Text -> Text -> Text -> IO ()
sendAnnouncementViaJson hook fieldName message = do
  let (Just (hookUrl, _)) = parseUrlHttps $ DTE.encodeUtf8 hook

  R.runReq def $ do
    let payload = object [ fieldName .= message ]
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
