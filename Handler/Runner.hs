
module Handler.Runner where

import Import
import System.Process
import System.Exit
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
runProg workingDir programPath args = Runner {
  runRunner = \chan -> do
      (code, _) <- runProgram workingDir programPath args chan
      case code of
        ExitSuccess -> return $ RunnerOK ()
        _ -> return $ RunnerError code
  }

runProgram :: Maybe FilePath -> FilePath -> [String] -> Channel -> Handler (ExitCode, Text)
runProgram workingDir programPath args chan = do
  (_, Just hout, Just herr, pid) <-
       liftIO $ createProcess (proc programPath args){ std_out = CreatePipe,
                                                       std_err = CreatePipe,
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

msg :: Channel -> Text -> Handler ()
msg chan m = liftIO $ atom $ writeTChan chan $ Just (m ++ "\n")

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
