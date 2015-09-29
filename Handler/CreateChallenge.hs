module Handler.CreateChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Handler.Shared
import Handler.Extract

import System.Directory (doesFileExist)
import System.FilePath.Find as SFF
import qualified Data.Text as T

import PersistSHA1

getCreateChallengeR :: Handler Html
getCreateChallengeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (Import.FileInfo, Text)
        handlerName = "getCreateChallengeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "create-challenge")

postCreateChallengeR :: Handler TypedContent
postCreateChallengeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postCreateChallengeR" :: Text
        challengeData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (name, publicUrl, publicBranch, privateUrl, privateBranch) = challengeData

    runViewProgress $ doCreateChallenge name publicUrl publicBranch privateUrl privateBranch

doCreateChallenge :: Text -> Text -> Text -> Text -> Text -> Channel -> Handler ()
doCreateChallenge name publicUrl publicBranch privateUrl privateBranch chan = do
  maybePublicRepoId <- cloneRepo publicUrl publicBranch chan
  case maybePublicRepoId of
    Just publicRepoId -> do
      maybePrivateRepoId <- cloneRepo privateUrl privateBranch chan
      case maybePrivateRepoId of
          Just privateRepoId -> addChallenge name publicRepoId privateRepoId chan
          Nothing -> return ()
    Nothing -> return ()

addChallenge :: Text -> (Key Repo) -> (Key Repo) -> Channel -> Handler ()
addChallenge name publicRepoId privateRepoId chan = do
  msg chan "adding challenge..."
  let publicRepoDir = getRepoDir publicRepoId
  let readmeFilePath = publicRepoDir </> readmeFile
  doesReadmeExist <- liftIO $ doesFileExist readmeFilePath
  (title, description) <- if doesReadmeExist
                           then
                            liftIO $ extractTitleAndDescription readmeFilePath
                           else do
                            err chan "README was not found"
                            return (defaultTitle, defaultDescription)
  time <- liftIO getCurrentTime
  challengeId <- runDB $ insert $ Challenge {
    challengePublicRepo=publicRepoId,
    challengePrivateRepo=privateRepoId,
    challengeName=name,
    challengeTitle=(T.pack $ title),
    challengeDescription=(T.pack $ description),
    challengeStamp=time}
  updateTests challengeId chan
  return ()

updateTests :: (Key Challenge) -> Channel -> Handler ()
updateTests challengeId chan = do
  challenge <- runDB $ get404 challengeId
  let repoId = challengePrivateRepo challenge
  let repoDir = getRepoDir repoId
  repo <- runDB $ get404 repoId
  let commit = repoCurrentCommit repo
  testDirs <- liftIO $ findTestDirs repoDir
  mapM_ (checkTestDir chan challengeId commit) testDirs
  msg chan (T.pack $ show testDirs)
  return ()

expectedFileName = "expected.tsv"

doesExpectedExist :: FilePath -> IO Bool
doesExpectedExist fp = doesFileExist (fp </> expectedFileName)

checkTestDir :: Channel -> (Key Challenge) -> SHA1 -> FilePath -> Handler ()
checkTestDir chan challengeId commit testDir = do
  expectedExists <- liftIO $ doesExpectedExist testDir
  if expectedExists
    then do
      msg chan $ concat ["Test dir ", (T.pack testDir), " found."]
      checksum <- liftIO $ gatherSHA1 testDir
      testId <- runDB $ insert $ Test {
        testChallenge=challengeId,
        testName=T.pack testDir,
        testChecksum=(SHA1 checksum),
        testCommit=commit,
        testActive=True }
      return ()
    else
      msg chan $ concat ["Test dir ", (T.pack testDir), " does not have expected results."]
  return ()


gatherSHA1 :: FilePath -> IO ByteString
gatherSHA1 testDir = do
  files <- SFF.find always isTestDirHashedFile testDir
  gatherSHA1ForCollectionOfFiles files

isTestDirHashedFile :: FindClause Bool
isTestDirHashedFile = fileType ==? RegularFile


findTestDirs :: FilePath -> IO [FilePath]
findTestDirs = SFF.find never testDirFilter

never :: FindClause Bool
never = depth ==? 0

testDirFilter :: FindClause Bool
testDirFilter = (fileType ==? Directory) &&? (SFF.fileName ~~? "dev-*" ||? SFF.fileName ~~? "test-*")

sampleForm :: Form (Text, Text, Text, Text, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> areq textField (fieldSettingsLabel MsgName) Nothing
    <*> areq textField (fieldSettingsLabel MsgPublicUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
    <*> areq textField (fieldSettingsLabel MsgPrivateUrl) Nothing
    <*> areq textField (fieldSettingsLabel MsgBranch) Nothing
