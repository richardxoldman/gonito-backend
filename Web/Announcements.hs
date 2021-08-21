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

data AnnouncementHook = SlackWebHook Text

toAnnouncementHook :: Text -> AnnouncementHook
toAnnouncementHook url
  | ".slack." `isInfixOf` url = SlackWebHook url
  | otherwise = error $ "unknown hook type"

sendAnnouncement :: AnnouncementHook -> Text -> IO ()
sendAnnouncement (SlackWebHook hook) message = do
  let (Just (hookUrl, _)) = parseUrlHttps $ DTE.encodeUtf8 hook

  R.runReq def $ do
    let payload = object [ "text" .= message ]
    (_ :: IgnoreResponse) <- R.req R.POST
                                 hookUrl
                                 (R.ReqBodyJson payload)
                                 R.ignoreResponse
                                 mempty
    return ()

formatLink :: Maybe AnnouncementHook -> Text -> Text -> Text
formatLink (Just (SlackWebHook _)) url title = "<" <> url <> "|" <> title <> ">"
formatLink Nothing url title = title <> "<" <> url <> ">"
