
module Handler.Runner where

import Import

import qualified System.IO as SIO

import System.Process
import System.Exit
import System.Environment
import Control.Concurrent.STM
import Control.Concurrent.Lifted (threadDelay)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class

type Channel = TChan (Maybe Text)

data RunnerStatus a = RunnerOK a | RunnerError ExitCode

newtype Runner a = Runner { runRunner :: Channel -> Handler (RunnerStatus a) }

getChannel :: Runner Channel
getChannel = Runner {
  runRunner = \chan -> return $ RunnerOK chan
  }

instance Functor Runner where
  fmap f runner = Runner {
     runRunner = \chan -> do
         s <- (runRunner runner) chan
         return $ case s of
           RunnerOK v -> RunnerOK $ f v
           RunnerError e -> RunnerError e
     }

instance Applicative Runner where
  pure v = Runner {
    runRunner = \_ -> return $ RunnerOK v
    }
  liftA2 f runner1 runner2 = Runner {
    runRunner = \chan -> do
        s1 <- (runRunner runner1) chan
        case s1 of
          RunnerOK v1 -> do
            s2 <- (runRunner runner2) chan
            case s2 of
              RunnerOK v2 -> return $ RunnerOK $ f v1 v2
              RunnerError e -> return $ RunnerError e
          RunnerError e -> return $ RunnerError e
    }

instance Monad Runner where
  runner >>= k = Runner {
    runRunner = \chan -> do
        s <- (runRunner runner) chan
        case s of
          RunnerError e -> return $ RunnerError e
          RunnerOK v -> do
            sn <- (runRunner (k v)) chan
            return $ case sn of
              RunnerError e -> RunnerError e
              RunnerOK w -> RunnerOK w
    }

instance MonadIO Runner where
  liftIO action = Runner {
    runRunner = \_ -> do
        r <- liftIO action
        return $ RunnerOK r
    }

runWithChannel :: Channel -> Runner () -> Handler ExitCode
runWithChannel chan runner = do
  s <- (runRunner runner) chan
  return $ case s of
             RunnerOK () -> ExitSuccess
             RunnerError e -> e

runProg :: Maybe FilePath -> FilePath -> [String] -> Runner ()
runProg workingDir programPath args = runProgWithEnv workingDir [] programPath args

runProgWithEnv :: Maybe FilePath -> [(String, String)] -> FilePath -> [String] -> Runner ()
runProgWithEnv workingDir extraEnv programPath args = Runner {
  runRunner = \chan -> do
      (code, _) <- runProgramWithEnv workingDir extraEnv programPath args chan
      case code of
        ExitSuccess -> return $ RunnerOK ()
        _ -> return $ RunnerError code
  }

runProgram :: Maybe FilePath -> FilePath -> [String] -> Channel -> Handler (ExitCode, Text)
runProgram workingDir programPath args chan =
  runProgramWithEnv workingDir [] programPath args chan

runProgramWithEnv :: Maybe FilePath -> [(String, String)] -> FilePath -> [String] -> Channel -> Handler (ExitCode, Text)
runProgramWithEnv workingDir extraEnv programPath args chan = do
  liftIO $ putStrLn $ pack $ show extraEnv
  liftIO $ putStrLn $ pack $ show args
  env <- liftIO $ getEnvironment
  (_, Just hout, Just herr, pid) <-
       liftIO $ createProcess (proc programPath args){
      std_out = CreatePipe,
      std_err = CreatePipe,
      -- https://serverfault.com/questions/544156/git-clone-fail-instead-of-prompting-for-credentials
      env = Just (("GIT_TERMINAL_PROMPT", "0") : (env ++ extraEnv)),
      cwd = workingDir}
  (code, out) <- gatherOutput pid hout herr chan
  _ <- liftIO $ waitForProcess pid
  return (code, out)

gatherOutput :: ProcessHandle -> Handle -> Handle -> Channel -> Handler (ExitCode, Text)
gatherOutput ph hout herr chan = work mempty mempty
  where
    work accout accerr = do
        -- Read any outstanding input.
        resterr <- takeABit herr accerr
        restout <- takeABit hout accout
        liftIO $ threadDelay 1000000
        -- Check on the process.
        s <- liftIO $ getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work restout resterr
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                _ <- takeFinalBit herr resterr
                allTheRest <- takeFinalBit hout restout
                return (ec, allTheRest)
    takeABit h acc = do
      bs <- liftIO $ BS.hGetNonBlocking h (64 * 1024)
      let acc' = acc <> (decodeUtf8 bs)
      let (fullLines, rest) = processOutput acc'
      mapM_ (msg chan) fullLines
      return rest
    takeFinalBit h rest = do
      lastPart <- liftIO $ BS.hGetContents h
      let allTheRest = rest <> (decodeUtf8 lastPart)
      mapM_ (msg chan) $ lines allTheRest
      return allTheRest

msg :: Channel -> Text -> Handler ()
msg chan m = do
  liftIO $ SIO.hPutStrLn stderr (unpack m)
  liftIO $ atom $ writeTChan chan $ Just (m ++ "\n")

err :: Channel -> Text -> Handler ()
err = msg

raw :: Channel -> Text -> Handler ()
raw = msg

atom = Control.Concurrent.STM.atomically

processOutput :: Text -> ([Text], Text)
processOutput = processOutput' . lines
                where processOutput' [] = ([], "")
                      processOutput' out = (init out, last out)
                      init [] = []
                      init [x] = []
                      init (x:xs) = (x:(init xs))
                      last [x] = x
                      last (_:xs) = last xs
