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
import System.Random

import PersistSHA1

import qualified Data.ByteString as BS

atom = Control.Concurrent.STM.atomically

type Channel = TChan (Maybe Text)

arena :: FilePath
arena = "arena"

gitPath :: FilePath
gitPath = "/usr/bin/git"

runViewProgress :: (Channel -> Handler ()) -> Handler TypedContent
runViewProgress action = do
  App {..} <- getYesod
  jobId <- randomInt
  chan <- liftIO $ atom $ do
    chan <- newBroadcastTChan
    m <- readTVar jobs
    writeTVar jobs $ IntMap.insert jobId chan m
    return chan
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

doRepoCloning :: Text -> Text -> Channel -> Handler ()
doRepoCloning url branch chan = do
  msg chan "Did something"
  _ <- cloneRepo url branch chan
  return ()

validGitProtocols :: [String]
validGitProtocols = ["git", "http", "https", "ssh"]

validGitProtocolsAsText :: Text
validGitProtocolsAsText = T.pack $ intercalate ", " $ map (++"://") validGitProtocols

cloneRepo :: Text -> Text -> Channel -> Handler (Maybe (Key Repo))
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
        r <- randomInt
        let repoDir = arena </> ("t" ++ show r)
        (exitCode, _) <- runProgram Nothing gitPath ["clone",
                                                     "--progress",
                                                     "--branch",
                                                     T.unpack branch,
                                                     T.unpack url,
                                                     repoDir] chan
        case exitCode of
          ExitSuccess -> do
            (exitCode, out) <- runProgram (Just repoDir) gitPath ["rev-parse", "HEAD"] chan
            case exitCode of
              ExitSuccess -> do
                msg chan $ concat ["HEAD commit is ", commitId]
                userId <- requireAuthId
                time <- liftIO getCurrentTime
                repoId <- runDB $ insert $ Repo {
                  repoUrl=url,
                  repoBranch=branch,
                  repoCurrentCommit=(toSHA1 (encodeUtf8 commitId)),
                  repoOwner=userId,
                  repoReady=True,
                  repoStamp=time }
                return $ Just repoId
                where commitId = T.replace "\n" "" out
              ExitFailure _ -> do
                err chan "cannot determine HEAD commit"
                return Nothing
          ExitFailure _ -> do
            err chan "git failed"
            return Nothing
       else do
        err chan $ concat ["Wrong URL to a Git repo (note that one of the following protocols must be specified: ", validGitProtocolsAsText]
        return Nothing

hexByteToWord8 :: Text -> Word8
hexByteToWord8 t = (hexNibbleToWord8 $ T.head t) * 16 + (hexNibbleToWord8 $ T.index t 1)

hexNibbleToWord8 :: Char -> Word8
hexNibbleToWord8 '0' = 0
hexNibbleToWord8 '1' = 1
hexNibbleToWord8 '2' = 2
hexNibbleToWord8 '3' = 3
hexNibbleToWord8 '4' = 4
hexNibbleToWord8 '5' = 5
hexNibbleToWord8 '6' = 6
hexNibbleToWord8 '7' = 7
hexNibbleToWord8 '8' = 8
hexNibbleToWord8 '9' = 9
hexNibbleToWord8 'A' = 10
hexNibbleToWord8 'a' = 10
hexNibbleToWord8 'B' = 11
hexNibbleToWord8 'b' = 11
hexNibbleToWord8 'C' = 12
hexNibbleToWord8 'c' = 12
hexNibbleToWord8 'D' = 13
hexNibbleToWord8 'd' = 13
hexNibbleToWord8 'E' = 14
hexNibbleToWord8 'e' = 14
hexNibbleToWord8 'F' = 15
hexNibbleToWord8 'f' = 15


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

runProgram :: Maybe FilePath -> FilePath -> [String] -> Channel -> Handler (ExitCode, Text)
runProgram workingDir programPath args chan = do
  (_, Just hout, Just herr, pid) <-
       liftIO $ createProcess (proc programPath args){ std_out = CreatePipe,
                                                       std_err = CreatePipe,
                                                       cwd = workingDir}
  (code, out) <- gatherOutput pid hout herr chan
  _ <- liftIO $ waitForProcess pid
  return (code, out)


processOutput :: Text -> ([Text], Text)
processOutput = processOutput' . lines
                where processOutput' [] = ([], "")
                      processOutput' out = (init out, last out)
                      init [] = []
                      init [x] = []
                      init (x:xs) = (x:(init xs))
                      last [x] = x
                      last (_:xs) = last xs


gatherOutput :: ProcessHandle -> Handle -> Handle -> Channel -> Handler (ExitCode, Text)
gatherOutput ph hout herr chan = work mempty mempty
  where
    work accout accerr = do
        -- Read any outstanding input.
        resterr <- takeABit herr accerr
        restout <- takeABit hout accout
        threadDelay 1000000
        -- Check on the process.
        s <- liftIO $ getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work restout resterr
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                _ <- takeFinalBit herr resterr
                all <- takeFinalBit hout restout
                return (ec, all)
    takeABit h acc = do
      bs <- liftIO $ BS.hGetNonBlocking hout (64 * 1024)
      let acc' = acc <> (decodeUtf8 bs)
      let (fullLines, rest) = processOutput acc'
      mapM_ (msg chan) fullLines
      return rest
    takeFinalBit h rest = do
      last <- liftIO $ BS.hGetContents h
      let all = rest <> (decodeUtf8 last)
      mapM_ (msg chan) $ lines all
      return all

randomInt :: Handler Int
randomInt = liftIO $ randomIO
