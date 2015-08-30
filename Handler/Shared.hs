{-# LANGUAGE RecordWildCards   #-}

module Handler.Shared where

import Import

import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap

import Network.URI
import qualified Data.Text as T

import Database.Persist.Sql (ConnectionPool, runSqlPool)

import Control.Concurrent.Lifted (fork, threadDelay)

import System.Process
import System.Exit

import qualified Data.ByteString as BS

atom = Control.Concurrent.STM.atomically

type Channel = TChan (Maybe Text)

runViewProgress :: (Channel -> Handler ()) -> Handler TypedContent
runViewProgress action = do
  App {..} <- getYesod
  (jobId, chan) <- liftIO $ atom $ do
    jobId <- readTVar nextJob
    writeTVar nextJob $! jobId + 1
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return (jobId, chan)
  fork $ do
    liftIO $ threadDelay 1000000
    action chan
    liftIO $ atom $ do
      writeTChan chan $ Just "All done\n"
      writeTChan chan Nothing
      m <- readTVar jobs
      writeTVar jobs $ IntMap.delete jobId m
  redirect $ ViewProgressR jobId

msg :: Channel -> Text -> Handler ()
msg chan m = liftIO $ atom $ writeTChan chan $ Just (m ++ "\n")

err :: Channel -> Text -> Handler ()
err = msg

raw :: Channel -> Text -> Handler ()
raw = msg


doSomething :: Channel -> Handler ()
doSomething chan = do
  msg chan "Did something"
  threadDelay 1000000
  msg chan "Did something else"
  threadDelay 1000000

doRepoCloning :: Text -> Text -> Channel -> Handler ()
doRepoCloning url branch chan = do
  msg chan "Did something"
  _ <- cloneRepo url branch chan
  return ()

validGitProtocols :: [String]
validGitProtocols = ["git", "http", "https", "ssh"]

validGitProtocolsAsText :: Text
validGitProtocolsAsText = T.pack $ intercalate ", " $ map (++"://") validGitProtocols

cloneRepo :: Text -> Text -> Channel -> Handler (Maybe Repo)
cloneRepo url branch chan = do
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  case maybeRepo of
    Just _ -> do
      err chan "Repo already there"
      return Nothing
    Nothing -> do
      msg chan $ concat ["Preparing to clone repo ", url]
      if checkRepoUrl url
       then do
        msg chan "Cloning..."
        runProgram "/usr/bin/git" ["clone",
                                   "--progress",
                                   T.unpack url] chan
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

runProgram :: FilePath -> [String] -> Channel -> Handler ()
runProgram programPath args chan = do
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode programPath args ""
  raw chan $ T.pack err
  raw chan $ T.pack out

--  (_, Just hout, Just herr, pid) <-
--       liftIO $ createProcess (proc programPath args){ std_out = CreatePipe, std_err = CreatePipe }

--  outErr <- liftIO $ hGetContents herr
--  let outErrLines = lines outErr
--  mapM_ (raw chan) outErrLines
--  (code, out) <- liftIO $ gatherOutput pid herr
--  raw chan $ decodeUtf8 out
--  _ <- liftIO $ waitForProcess pid
--  return ()


processOutput :: Text -> ([Text], Text)
processOutput = processOutput' . lines
                where processOutput' [] = ([], "")
                      processOutput' out = (init out, last out)
                      init [] = []
                      init [x] = []
                      init (x:xs) = (x:(init xs))
                      last [x] = x
                      last (_:xs) = last xs


gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, ByteString)
gatherOutput ph h = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        bs <- BS.hGetNonBlocking h (64 * 1024)
        let acc' = acc <> bs
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work acc'
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                last <- BS.hGetContents h
                return (ec, acc' <> last)
