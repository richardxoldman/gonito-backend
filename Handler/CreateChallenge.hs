module Handler.CreateChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

import Handler.Shared
import Handler.Runner
import Handler.Extract

import GEval.Core
import GEval.OptionsParser

import System.Directory (doesFileExist)
import System.FilePath.Find as SFF
import System.FilePath
import qualified Data.Text as T

import PersistSHA1

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Conduit.Binary (sinkLbs, sourceFile)

getCreateChallengeR :: Handler Html
getCreateChallengeR = do
    (formWidget, formEnctype) <- generateFormPost createChallengeForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "create-challenge")

postCreateChallengeR :: Handler TypedContent
postCreateChallengeR = do
    ((result, _), _) <- runFormPost createChallengeForm
    let challengeData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (name, publicUrl, publicBranch, publicGitAnnexRemote,
                    privateUrl, privateBranch, privateGitAnnexRemote) = challengeData

    userId <- requireAuthId
    user <- runDB $ get404 userId
    if userIsAdmin user
      then
       do
        let name' = T.strip name

        if isLocalIdAcceptable name'
          then
            runViewProgress $ doCreateChallenge name'
                                                (T.strip publicUrl)
                                                (T.strip publicBranch)
                                                (T.strip <$> publicGitAnnexRemote)
                                                (T.strip privateUrl)
                                                (T.strip privateBranch)
                                                (T.strip <$> privateGitAnnexRemote)
          else
            runViewProgress $ (flip err) "unexpected challenge ID (use only lower-case letters, digits and hyphens, start with a letter)"
      else
        runViewProgress $ (flip err) "MUST BE AN ADMIN TO CREATE A CHALLENGE"

doCreateChallenge :: Text -> Text -> Text -> Maybe Text -> Text -> Text -> Maybe Text -> Channel -> Handler ()
doCreateChallenge name publicUrl publicBranch publicGitAnnexRemote privateUrl privateBranch privateGitAnnexRemote chan = do
  maybePublicRepoId <- cloneRepo (RepoCloningSpec {
                                    cloningSpecRepo = RepoSpec {
                                        repoSpecUrl = publicUrl,
                                          repoSpecBranch = publicBranch,
                                          repoSpecGitAnnexRemote = publicGitAnnexRemote},
                                      cloningSpecReferenceRepo = RepoSpec {
                                        repoSpecUrl = publicUrl,
                                          repoSpecBranch = publicBranch,
                                          repoSpecGitAnnexRemote = publicGitAnnexRemote}}) chan
  case maybePublicRepoId of
    Just publicRepoId -> do
      publicRepo <- runDB $ get404 publicRepoId
      publicRepoDir <- getRepoDir publicRepoId
      maybePrivateRepoId <- cloneRepo (RepoCloningSpec {
                                         cloningSpecRepo = RepoSpec {
                                             repoSpecUrl = privateUrl,
                                             repoSpecBranch = privateBranch,
                                             repoSpecGitAnnexRemote = privateGitAnnexRemote},
                                         cloningSpecReferenceRepo = RepoSpec {
                                             repoSpecUrl =(T.pack $ publicRepoDir),
                                             repoSpecBranch = (repoBranch publicRepo),
                                             repoSpecGitAnnexRemote = (repoGitAnnexRemote publicRepo)}}) chan
      case maybePrivateRepoId of
        Just privateRepoId -> addChallenge name publicRepoId privateRepoId chan
        Nothing -> return ()
    Nothing -> return ()

addChallenge :: Text -> (Key Repo) -> (Key Repo) -> Channel -> Handler ()
addChallenge name publicRepoId privateRepoId chan = do
  msg chan "adding challenge..."
  publicRepoDir <- getRepoDir publicRepoId
  let readmeFilePath = publicRepoDir </> readmeFile
  doesReadmeExist <- liftIO $ doesFileExist readmeFilePath
  (title, description) <- if doesReadmeExist
                           then
                            liftIO $ extractTitleAndDescription readmeFilePath
                           else do
                            err chan "README was not found"
                            return (defaultTitle, defaultDescription)

  let imageFilePath = publicRepoDir </> imageFile
  doesImageFileExists <- liftIO $ doesFileExist imageFilePath
  mImage <- if doesImageFileExists
             then do
               fileBytes <- liftIO $ runResourceT $ sourceFile imageFilePath $$ sinkLbs
               return $ Just (S.pack . L.unpack $ fileBytes)
             else do
               return Nothing

  time <- liftIO getCurrentTime
  challengeId <- runDB $ insert $ Challenge {
    challengePublicRepo=publicRepoId,
    challengePrivateRepo=privateRepoId,
    challengeName=name,
    challengeTitle=(T.pack $ title),
    challengeDescription=(T.pack $ description),
    challengeStamp=time,
    challengeImage=mImage,
    challengeStarred=False,
    challengeArchived=Just False}
  updateTests challengeId chan
  return ()

updateTests :: (Key Challenge) -> Channel -> Handler ()
updateTests challengeId chan = do
  challenge <- runDB $ get404 challengeId
  let repoId = challengePrivateRepo challenge
  repoDir <- getRepoDir repoId
  repo <- runDB $ get404 repoId
  let commit = repoCurrentCommit repo
  testDirs <- liftIO $ findTestDirs repoDir
  mapM_ (checkTestDir chan challengeId challenge commit) testDirs
  msg chan (T.pack $ show testDirs)
  return ()

expectedFileName :: FilePath
expectedFileName = "expected.tsv"

doesExpectedExist :: FilePath -> IO Bool
doesExpectedExist fp = do
  efs <- mapM (\ext -> findFilePossiblyCompressed (fp </> expectedFileName -<.> ext)) extensionsHandled
  return $ not $ null $ catMaybes efs

checkTestDir :: Channel -> (Key Challenge) -> Challenge -> SHA1 -> FilePath -> Handler ()
checkTestDir chan challengeId challenge commit testDir = do
  expectedExists <- liftIO $ doesExpectedExist testDir
  if expectedExists
    then do
      msg chan $ concat ["Test dir ", (T.pack testDir), " found."]
      checksum <- liftIO $ gatherSHA1 testDir
      challengeRepoDir <- getRepoDir $ challengePrivateRepo challenge
      optionsParsingResult <- liftIO $ getOptions [
        "--expected-directory", challengeRepoDir,
        "--test-name", takeFileName testDir]
      case optionsParsingResult of
       Left _ -> do
         err chan "Cannot read metric"
         return ()
       Right opts -> do
         _ <- runDB $ mapM (\(priority, metric) -> insert $ Test {
           testChallenge=challengeId,
           testMetric=metric,
           testName=T.pack $ takeFileName testDir,
           testChecksum=(SHA1 checksum),
           testCommit=commit,
           testActive=True,
           testPrecision=gesPrecision $ geoSpec opts,
           testPriority=Just priority}) $ zip [1..] (gesMetrics $ geoSpec opts)
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

createChallengeForm :: Form (Text, Text, Text, Maybe Text, Text, Text, Maybe Text)
createChallengeForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> areq textField (fieldWithTooltip MsgChallengeName MsgChallengeNameTooltip) Nothing
    <*> areq textField (bfs MsgPublicUrl) Nothing
    <*> areq textField (bfs MsgBranch) (Just "master")
    <*> aopt textField (bfs MsgGitAnnexRemote) Nothing
    <*> areq textField (bfs MsgPrivateUrl) Nothing
    <*> areq textField (bfs MsgBranch) (Just "dont-peek")
    <*> aopt textField (bfs MsgGitAnnexRemote) Nothing
