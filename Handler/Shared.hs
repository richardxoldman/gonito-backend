{-# LANGUAGE RecordWildCards   #-}

module Handler.Shared where

import Import

import           Control.Concurrent.STM
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap

import Network.URI
import qualified Data.Text as T

import Database.Persist.Sql (ConnectionPool, runSqlPool, fromSqlKey)

import Control.Concurrent.Lifted (fork, threadDelay)

import System.Process
import System.Exit
import System.Random

import System.Directory (renameDirectory)

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
    Nothing -> cloneRepo' url branch chan

updateRepo :: Key Repo -> Channel -> Handler Bool
updateRepo repoId chan = do
  repo <- runDB $ get404 repoId
  let repoDir = getRepoDir repoId
  (exitCode, _) <- runProgram (Just repoDir) gitPath ["pull", "--progress"] chan
  case exitCode of
    ExitSuccess -> do
      maybeHeadCommit <- getHeadCommit repoDir chan
      case maybeHeadCommit of
        Just headCommit -> do
          runDB $ update repoId [RepoCurrentCommit =. headCommit]
          return True
        Nothing -> return False

getHeadCommit :: FilePath -> Channel -> Handler (Maybe SHA1)
getHeadCommit repoDir chan = do
  (exitCode, out) <- runProgram (Just repoDir) gitPath ["rev-parse", "HEAD"] chan
  case exitCode of
    ExitSuccess -> do
      msg chan $ concat ["HEAD commit is ", commitId]
      return $ Just commitRaw
        where commitId = T.replace "\n" "" out
              commitRaw = fromTextToSHA1 commitId
    ExitFailure _ -> do
      err chan "cannot determine HEAD commit"
      return Nothing

cloneRepo' :: Text -> Text -> Channel -> Handler (Maybe (Key Repo))
cloneRepo' url branch chan = do
      msg chan $ concat ["Preparing to clone repo ", url]
      if checkRepoUrl url
       then do
        msg chan "Cloning..."
        r <- randomInt
        let tmpRepoDir = arena </> ("t" ++ show r)
        (exitCode, _) <- runProgram Nothing gitPath ["clone",
                                                     "--progress",
                                                     "--branch",
                                                     T.unpack branch,
                                                     T.unpack url,
                                                     tmpRepoDir] chan
        case exitCode of
          ExitSuccess -> do
            maybeHeadCommit <- getHeadCommit tmpRepoDir chan
            case maybeHeadCommit of
              Just commitRaw -> do
                userId <- requireAuthId
                time <- liftIO getCurrentTime
                repoId <- runDB $ insert $ Repo {
                  repoUrl=url,
                  repoBranch=branch,
                  repoCurrentCommit=commitRaw,
                  repoOwner=userId,
                  repoReady=True,
                  repoStamp=time }
                let repoDir = getRepoDir repoId
                liftIO $ renameDirectory tmpRepoDir repoDir
                msg chan $ concat ["Repo is in ", (T.pack repoDir)]
                return $ Just repoId
              Nothing -> do
                return Nothing
          ExitFailure _ -> do
            err chan "git failed"
            return Nothing
       else do
        err chan $ concat ["Wrong URL to a Git repo (note that one of the following protocols must be specified: ", validGitProtocolsAsText]
        return Nothing


getRepoDir :: Key Repo -> FilePath
getRepoDir repoId = arena </> ("r" ++ repoIdAsString)
                    where repoIdAsString = show $ fromSqlKey repoId

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
