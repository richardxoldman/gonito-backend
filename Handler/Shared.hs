{-# LANGUAGE RecordWildCards   #-}

module Handler.Shared where

import Import

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap

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
    action chan
    atom $ do
      writeTChan chan $ Just "All done\n"
      writeTChan chan Nothing
      m <- readTVar jobs
      writeTVar jobs $ IntMap.delete jobId m
  redirect $ ViewProgressR jobId

msg :: Channel -> Text -> IO ()
msg chan m = atom $ writeTChan chan $ Just m

doSomething :: Channel -> IO ()
doSomething chan = do
  threadDelay 1000000
  msg chan "Did something\n"
  threadDelay 1000000
  msg chan "Did something else\n"
  threadDelay 1000000


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
