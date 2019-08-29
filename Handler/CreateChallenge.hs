module Handler.CreateChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              bfs)

import Handler.Shared
import Handler.Runner
import Handler.Extract

import GEval.Core
import GEval.OptionsParser

import Gonito.ExtractMetadata (getLastCommitMessage)

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

data ChallengeUpdateType = MajorChange | MinorChange | ChallengePatch
                           deriving (Eq, Enum, Bounded)

instance Show ChallengeUpdateType where
  show MajorChange = "major change"
  show MinorChange = "minor change"
  show ChallengePatch = "patch"

getChallengeUpdateR :: ChallengeId -> Handler Html
getChallengeUpdateR challengeId = do
    (formWidget, formEnctype) <- generateFormPost updateChallengeForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "update-challenge")

postChallengeUpdateR :: ChallengeId -> Handler TypedContent
postChallengeUpdateR challengeId = do
    ((result, _), _) <- runFormPost updateChallengeForm
    let challengeData = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        Just (updateType, publicUrl, publicBranch, publicGitAnnexRemote,
                          privateUrl, privateBranch, privateGitAnnexRemote) = challengeData

    userId <- requireAuthId
    user <- runDB $ get404 userId
    if userIsAdmin user
      then
       do
         runViewProgress $ doChallengeUpdate challengeId updateType publicUrl publicBranch publicGitAnnexRemote privateUrl privateBranch privateGitAnnexRemote
      else
        runViewProgress $ (flip err) "MUST BE AN ADMIN TO UPDATE A CHALLENGE"

doChallengeUpdate :: ChallengeId -> ChallengeUpdateType -> Text -> Text -> Maybe Text -> Text -> Text -> Maybe Text -> Channel -> Handler ()
doChallengeUpdate challengeId updateType publicUrl publicBranch publicGitAnnexRemote privateUrl privateBranch privateGitAnnexRemote chan = do
  challenge <- runDB $ get404 challengeId
  (Entity _ version) <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  let (newMajor, newMinor, newPatch) = incrementVersion updateType (versionMajor version,
                                                                    versionMinor version,
                                                                    versionPatch version)

  msg chan ("UPDATING TO VERSION: " ++ (pack $ show newMajor) ++ "." ++ (pack $ show newMinor) ++ "." ++ (pack $ show newPatch))

  userId <- requireAuthId
  (Just publicRepoId) <- getPossiblyExistingRepo (\_ _ _ -> return True)
                                                userId
                                                challengeId
                                                RepoSpec {
                                                  repoSpecUrl = publicUrl,
                                                  repoSpecBranch = publicBranch,
                                                  repoSpecGitAnnexRemote = publicGitAnnexRemote}
                                                chan

  (Just privateRepoId) <- getPossiblyExistingRepo (\_ _ _ -> return True)
                                                 userId
                                                 challengeId
                                                 RepoSpec {
                                                   repoSpecUrl = privateUrl,
                                                   repoSpecBranch = privateBranch,
                                                   repoSpecGitAnnexRemote = privateGitAnnexRemote}
                                                 chan

  privateRepo <- runDB $ get404 $ privateRepoId
  repoDir <- getRepoDir privateRepoId
  (Just versionDescription) <- liftIO $ getLastCommitMessage repoDir
  theNow <- liftIO getCurrentTime
  let commit = (repoCurrentCommit privateRepo)

  mAlreadyExistingVersion <- runDB $ getBy $ UniqueVersionByCommit commit
  case mAlreadyExistingVersion of
    Just (Entity versionId _) -> do
                                 runDB $ update versionId [VersionMajor =. newMajor,
                                                           VersionMinor =. newMinor,
                                                           VersionPatch =. newPatch,
                                                           VersionDescription =. versionDescription,
                                                           VersionStamp =. theNow]

    Nothing -> do
               _ <- runDB $ insert $ Version commit
                           newMajor
                           newMinor
                           newPatch
                           versionDescription
                           theNow
               return ()

  (title, description, mImage) <- extractChallengeMetadata publicRepoId chan

  runDB $ update challengeId [ChallengePublicRepo =. publicRepoId,
                              ChallengePrivateRepo =. privateRepoId,
                              ChallengeVersion =. commit,
                              ChallengeTitle =. title,
                              ChallengeDescription =. description,
                              ChallengeImage =. mImage]

  updateTests challengeId chan

  return ()


incrementVersion :: ChallengeUpdateType -> (Int, Int, Int) -> (Int, Int, Int)
incrementVersion MajorChange (major, minor, patch) = (major + 1, minor, patch)
incrementVersion MinorChange (major, minor, patch) = (major, minor + 1, patch)
incrementVersion ChallengePatch (major, minor, patch) = (major, minor, patch + 1)


defaultMajorVersion :: Int
defaultMajorVersion = 1

defaultMinorVersion :: Int
defaultMinorVersion = 0

defaultPatchVersion :: Int
defaultPatchVersion = 0

defaultInitialDescription :: Text
defaultInitialDescription = "initial version"

extractChallengeMetadata :: Key Repo -> Channel -> Handler (Text, Text, Maybe ByteString)
extractChallengeMetadata publicRepoId chan = do
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

  return (T.pack $ title, T.pack $ description, mImage)

addChallenge :: Text -> (Key Repo) -> (Key Repo) -> Channel -> Handler ()
addChallenge name publicRepoId privateRepoId chan = do
  msg chan "adding challenge..."

  (title, description, mImage) <- extractChallengeMetadata publicRepoId chan

  privateRepo <- runDB $ get404 privateRepoId
  time <- liftIO getCurrentTime

  let commit=repoCurrentCommit $ privateRepo

  _ <- runDB $ insert $ Version {
    versionCommit=commit,
    versionMajor=defaultMajorVersion,
    versionMinor=defaultMinorVersion,
    versionPatch=defaultPatchVersion,
    versionDescription=defaultInitialDescription,
    versionStamp=time}

  challengeId <- runDB $ insert $ Challenge {
    challengePublicRepo=publicRepoId,
    challengePrivateRepo=privateRepoId,
    challengeName=name,
    challengeTitle=title,
    challengeDescription=description,
    challengeStamp=time,
    challengeImage=mImage,
    challengeStarred=False,
    challengeArchived=Just False,
    challengeVersion=commit}

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
         _ <- runDB $ mapM (insertOrUpdateTest testDir challengeId (SHA1 checksum) commit opts)  $ zip [1..] (gesMetrics $ geoSpec opts)
         return ()
    else
      msg chan $ concat ["Test dir ", (T.pack testDir), " does not have expected results."]
  return ()

insertOrUpdateTest testDir challengeId checksum commit opts (priority, metric) = do
  let name=T.pack $ takeFileName testDir
  mAlreadyExistingTest <- getBy $ UniqueChallengeNameMetricChecksum challengeId name metric checksum
  case mAlreadyExistingTest of
    Just (Entity testId _) -> update testId [TestCommit=.commit,
                                            TestPrecision=.(gesPrecision $ geoSpec opts),
                                            TestPriority=.Just priority]
    Nothing -> do
                _ <- insert $ Test {
                  testChallenge=challengeId,
                  testMetric=metric,
                  testName=name,
                  testChecksum=checksum,
                  testCommit=commit,
                  testActive=True,
                  testPrecision=gesPrecision $ geoSpec opts,
                  testPriority=Just priority}
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

updateChallengeForm :: Form (ChallengeUpdateType, Text, Text, Maybe Text, Text, Text, Maybe Text)
updateChallengeForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> areq (radioField optionsEnum) "change type" (Just ChallengePatch)
    <*> areq textField (bfs MsgPublicUrl) Nothing
    <*> areq textField (bfs MsgBranch) (Just "master")
    <*> aopt textField (bfs MsgGitAnnexRemote) Nothing
    <*> areq textField (bfs MsgPrivateUrl) Nothing
    <*> areq textField (bfs MsgBranch) (Just "dont-peek")
    <*> aopt textField (bfs MsgGitAnnexRemote) Nothing
