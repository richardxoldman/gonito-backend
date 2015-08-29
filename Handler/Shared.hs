{-# LANGUAGE RecordWildCards   #-}

module Handler.Shared where

import Import

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap

import Network.URI
import qualified Data.Text as T

import Database.Persist.Sql (ConnectionPool, runSqlPool)

atom = Control.Concurrent.STM.atomically

type Channel = TChan (Maybe Text)

runViewProgress :: (Channel -> IO ()) -> Handler TypedContent
runViewProgress action = do
  App {..} <- getYesod
  (jobId, chan) <- liftIO $ atom $ do
    jobId <- readTVar nextJob
    writeTVar nextJob $! jobId + 1
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return (jobId, chan)
  liftIO $ forkIO $ do
    threadDelay 1000000
    action chan
    atom $ do
      writeTChan chan $ Just "All done\n"
      writeTChan chan Nothing
      m <- readTVar jobs
      writeTVar jobs $ IntMap.delete jobId m
  redirect $ ViewProgressR jobId

msg :: Channel -> Text -> IO ()
msg chan m = atom $ writeTChan chan $ Just (m ++ "\n")

err :: Channel -> Text -> IO ()
err = msg

doSomething :: Channel -> IO ()
doSomething chan = do
  msg chan "Did something"
  threadDelay 1000000
  msg chan "Did something else"
  threadDelay 1000000

doRepoCloning :: Text -> Text -> Channel -> IO ()
doRepoCloning url branch chan = do
  msg chan "Did something"
  _ <- cloneRepo url branch chan
  return ()

validGitProtocols :: [String]
validGitProtocols = ["git", "http", "https", "ssh"]

validGitProtocolsAsText :: Text
validGitProtocolsAsText = T.pack $ intercalate ", " $ map (++"://") validGitProtocols

cloneRepo :: Text -> Text -> Channel -> IO (Maybe Repo)
cloneRepo url branch chan = do
  let maybeRepo = Nothing
  case maybeRepo of
    Just _ -> do
      err chan "Repo already there"
      return Nothing
    Nothing -> do
      msg chan $ concat ["Preparing to clone repo ", url]
      if checkRepoUrl url
       then
        return Nothing
       else do
        err chan $ concat ["Wrong URL to a Git repo (note that one of the following protocols must be specified: ", validGitProtocolsAsText]
        return Nothing

checkRepoUrl :: Text -> Bool
checkRepoUrl url = case parsedURI of
                      Just uri -> (uriScheme uri) `elem` (map (++":") validGitProtocols)
                      Nothing -> False
                   where parsedURI = parseURI $ T.unpack url

getViewProgressR :: Int -> Handler TypedContent
getViewProgressR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atom $ do
        m <- readTVar jobs
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> fmap Just $ dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> respondSource typePlain $ do
            let loop = do
                    mtext <- liftIO $ atom $ readTChan chan
                    case mtext of
                        Nothing -> return ()
                        Just text -> do
                            sendChunkText text
                            sendFlush
                            loop
            loop
