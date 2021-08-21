{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Announcements
  (sendAnnouncement,
   formatLink)
  where

import Data.Text
import qualified Data.Text.Encoding as DTE
import Data.Maybe
import Network.HTTP.Req as R
import Prelude
import Data.Aeson
import Data.Default


sendAnnouncement :: Text -> Text -> IO ()
sendAnnouncement hook message = do
  let (Just (hookUrl, _)) = parseUrlHttps $ DTE.encodeUtf8 hook

  R.runReq def $ do
    let payload = object [ "text" .= message ]
    (_ :: IgnoreResponse) <- R.req R.POST
                                 hookUrl
                                 (R.ReqBodyJson payload)
                                 R.ignoreResponse
                                 mempty
    return ()

formatLink :: Text -> Text -> Text
formatLink url title = "<" <> url <> "|" <> title <> ">"
