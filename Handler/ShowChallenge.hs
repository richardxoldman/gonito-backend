module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, bfs)

import Data.Monoid

import qualified Data.Text.Lazy as TL
import           Text.Markdown

import System.Directory (doesFileExist)
import qualified Data.Text as T

import qualified Yesod.Table as Table

import Handler.Extract
import Handler.Shared
import Handler.Runner
import Handler.Tables
import Handler.TagUtils

import GEval.Core
import GEval.OptionsParser

import qualified Data.Map as Map

import PersistSHA1

import Options.Applicative

import System.IO (readFile)

import Data.Attoparsec.Text

import Data.Text (pack, unpack)

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  Just repo <- runDB $ get $ challengePublicRepo challenge
  (mainTest, leaderboard) <- getLeaderboardEntries challengeId
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  challengeLayout True challenge (showChallengeWidget muserId challenge scheme challengeRepo mainTest repo leaderboard)

getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  readme <- challengeReadme name
  challengeLayout False challenge $ toWidget readme

challengeReadme name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  contents <- liftIO $ System.IO.readFile readmeFilePath
  return $ markdown def $ TL.pack contents

showChallengeWidget muserId challenge scheme challengeRepo test repo leaderboard = $(widgetFile "show-challenge")
  where leaderboardWithRanks = zip [1..] leaderboard
        maybeRepoLink = getRepoLink repo


getRepoLink :: Repo -> Maybe Text
getRepoLink repo
  | sitePrefix `isPrefixOf` url = Just $ (browsableGitRepo bareRepoName) ++ "/" ++ (repoBranch repo)
  | otherwise = Nothing
  where sitePrefix = "git://gonito.net/" :: Text
        sitePrefixLen = length sitePrefix
        url = repoUrl repo
        bareRepoName = drop sitePrefixLen url

getChallengeHowToR :: Text -> Handler Html
getChallengeHowToR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  maybeUser <- maybeAuth

  app <- getYesod
  let settings = appSettings app

  let publicRepoId = challengePublicRepo challenge
  repo <- runDB $ get404 publicRepoId

  case maybeUser of
    Just (Entity userId user) -> do
      enableTriggerToken userId (userTriggerToken user)
    Nothing -> return ()

  let mToken = case maybeUser of
        Just (Entity _ user) -> userTriggerToken user
        Nothing -> Nothing

  let isIDSet = case maybeUser of
                  Just (Entity _ user) -> isJust $ userLocalId user
                  Nothing -> False
  isSSHUploaded <- case maybeUser of
    Just (Entity userId _) -> do
      keys <- runDB $ selectList [PublicKeyUser ==. userId] []
      return $ not (null keys)
    Nothing -> return False
  challengeLayout False challenge (challengeHowTo challenge settings repo (idToBeShown challenge maybeUser) isIDSet isSSHUploaded mToken)

idToBeShown challenge maybeUser =
  case maybeUser of
   Just user ->  case userLocalId $ entityVal user of
                 Just localId -> localId
                 Nothing -> defaultIdToBe
   Nothing -> defaultIdToBe
  where defaultIdToBe = "YOURID" :: Text

defaultRepo SelfHosted challenge _ maybeUser = "ssh://gitolite@gonito.net/" ++ (idToBeShown challenge maybeUser) ++ "/" ++ (challengeName challenge)
defaultRepo Branches _ repo _ = repoUrl repo

defaultBranch SelfHosted = Just "master"
defaultBranch Branches = Nothing

challengeHowTo challenge settings repo idToBeShown isIDSet isSSHUploaded mToken = $(widgetFile "challenge-how-to")

getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR name = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
   maybeUser <- maybeAuth

   Just repo <- runDB $ get $ challengePublicRepo challenge
   app <- getYesod
   let scheme = appRepoScheme $ appSettings app

   (formWidget, formEnctype) <- generateFormPost $ submissionForm (Just $ defaultRepo scheme challenge repo maybeUser) (defaultBranch scheme) (repoGitAnnexRemote repo)
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR name = do
    (Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
    ((result, formWidget), formEnctype) <- runFormPost $ submissionForm Nothing Nothing Nothing
    let submissionData = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just (mDescription, mTags, submissionUrl, submissionBranch, submissionGitAnnexRemote) = submissionData

    userId <- requireAuthId
    runViewProgress $ doCreateSubmission userId challengeId mDescription mTags RepoSpec {
                                                                                  repoSpecUrl=submissionUrl,
                                                                                  repoSpecBranch=submissionBranch,
                                                                                  repoSpecGitAnnexRemote=submissionGitAnnexRemote}

postTriggerLocallyR :: Handler TypedContent
postTriggerLocallyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just localId) <- lookupPostParam "user"
  mBranch <- lookupPostParam "branch"
  [Entity userId _] <- runDB $ selectList [UserLocalId ==. Just localId] []
  let localRepo = gitServer ++ localId ++ "/" ++ challengeName
  trigger userId challengeName localRepo mBranch

postTriggerRemotelyR :: Handler TypedContent
postTriggerRemotelyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just url) <- lookupPostParam "url"
  (Just token) <- lookupPostParam "token"
  mBranch <- lookupPostParam "branch"
  [Entity userId _] <- runDB $ selectList [UserTriggerToken ==. Just token] []
  trigger userId challengeName url mBranch

trigger :: UserId -> Text -> Text -> Maybe Text -> Handler TypedContent
trigger userId challengeName url mBranch = do
  let branch = fromMaybe "master" mBranch
  mChallengeEnt <- runDB $ getBy $ UniqueName challengeName
  case mChallengeEnt of
    Just (Entity challengeId _) -> runOpenViewProgress $ doCreateSubmission userId challengeId
                                                                           Nothing Nothing
                                                                           RepoSpec {repoSpecUrl=url,
                                                                                     repoSpecBranch=branch,
                                                                                     repoSpecGitAnnexRemote=Nothing}
    Nothing -> return $ toTypedContent (("Unknown challenge `" ++ (Data.Text.unpack challengeName) ++ "`. Cannot be triggered, must be submitted manually at Gonito.net!\n") :: String)

doCreateSubmission :: UserId -> Key Challenge -> Maybe Text -> Maybe Text -> RepoSpec -> Channel -> Handler ()
doCreateSubmission userId challengeId mDescription mTags repoSpec chan = do
  maybeRepoKey <- getSubmissionRepo challengeId repoSpec chan
  case maybeRepoKey of
    Just repoId -> do
      repo <- runDB $ get404 repoId

      repoDir <- getRepoDir repoId
      commitMessage <- getLastCommitMessage repoDir chan
      let (mCommitDescription, mCommitTags) = parseCommitMessage commitMessage

      submissionId <- getSubmission userId repoId (repoCurrentCommit repo) challengeId (fromMaybe (fromMaybe "???" mCommitDescription) mDescription) chan
      _ <- getOuts chan submissionId

      runDB $ addTags submissionId (if isNothing mTags then mCommitTags else mTags) []
      msg chan "Done"
    Nothing -> return ()

getSubmission :: UserId -> Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission userId repoId commit challengeId description chan = do
  maybeSubmission <- runDB $ getBy $ UniqueSubmissionRepoCommitChallenge repoId commit challengeId
  case maybeSubmission of
    Just (Entity submissionId _) -> do
      msg chan "Submission already there, re-checking"
      return submissionId
    Nothing -> do
      msg chan "Creating new submission"
      time <- liftIO getCurrentTime
      runDB $ insert $ Submission {
        submissionRepo=repoId,
        submissionCommit=commit,
        submissionChallenge=challengeId,
        submissionDescription=description,
        submissionStamp=time,
        submissionSubmitter=userId,
        submissionIsPublic=False }

parseCommitMessage :: Maybe Text -> (Maybe Text, Maybe Text)
parseCommitMessage Nothing = (Nothing, Nothing)
parseCommitMessage (Just commitMessage) =
  case parseOnly commitMessageParser commitMessage of
    Left _ -> (Nothing, Nothing)
    Right (d, ts) -> (d, ts)

commitMessageParser :: Data.Attoparsec.Text.Parser (Maybe Text, Maybe Text)
commitMessageParser = do
  skipMany emptyLine
  d <- nonEmptyLine
  mTs <- (do
          ts <- findTagsLine
          return $ Just ts) <|> (return Nothing)
  return (Just d, mTs)

findTagsLine :: Data.Attoparsec.Text.Parser Text
findTagsLine = tagsLine <|> (anyLine >> findTagsLine)

tagsLine :: Data.Attoparsec.Text.Parser Text
tagsLine = do
  (string "tags" <|> string "labels" <|> string "Tags" <|> string "Labels")
  char ':'
  skipMany space
  s <- many notEndOfLine
  endOfLine
  return $ Data.Text.pack s

commaSep p  = p `sepBy` (skipMany space *> char ',' *> skipMany space)

nonEmptyLine :: Data.Attoparsec.Text.Parser Text
nonEmptyLine = do
  skipMany space
  l1 <- notSpace
  l <- (many notEndOfLine)
  endOfLine
  return $ Data.Text.pack (l1:l)

anyLine :: Data.Attoparsec.Text.Parser ()
anyLine = do
  skipMany notEndOfLine
  endOfLine

notSpace :: Data.Attoparsec.Text.Parser Char
notSpace = satisfy (\c -> c /= '\r' && c /= '\n' && c /= ' ' && c /= '\t')

notEndOfLine :: Data.Attoparsec.Text.Parser Char
notEndOfLine = satisfy (\c -> c /= '\r' && c /= '\n')

emptyLine = do
  many space
  endOfLine

getOuts :: Channel -> Key Submission -> Handler ([Out])
getOuts chan submissionId = do
  submission <- runDB $ get404 submissionId
  let challengeId = submissionChallenge submission
  repoDir <- getRepoDir $ submissionRepo submission
  activeTests <- runDB $ selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  testsDone <- filterM (liftIO . doesOutExist repoDir) activeTests
  outs <- mapM (outForTest repoDir submissionId) testsDone
  mapM_ checkOrInsertOut outs
  mapM_ (checkOrInsertEvaluation repoDir chan) outs
  return outs

outFileName = "out.tsv"

getOutFilePath repoDir test = repoDir </> (T.unpack $ testName test) </> outFileName

findOutFile repoDir test = do
  let baseOut = getOutFilePath repoDir test
  let possibleOuts = [baseOut] ++ (map (baseOut <.>)  ["gz", "bz2", "xz"])
  foundFiles <- filterM doesFileExist possibleOuts
  return $ case foundFiles of
    [] -> Nothing
    (h:_) -> Just h

doesOutExist repoDir (Entity _ test) = do
  result <- findOutFile repoDir test
  return $ isJust result

outForTest repoDir submissionId (Entity testId test) = do
  (Just outF) <- liftIO $ findOutFile repoDir test
  checksum <- liftIO $ gatherSHA1ForCollectionOfFiles [outF]
  return Out {
    outSubmission=submissionId,
    outTest=testId,
    outChecksum=SHA1 checksum }

checkOrInsertOut :: Out -> Handler ()
checkOrInsertOut out = do
  maybeOut <- runDB $ getBy $ UniqueOutSubmissionTestChecksum (outSubmission out) (outTest out) (outChecksum out)
  case maybeOut of
    Just _ -> return ()
    Nothing -> (runDB $ insert out) >> return ()

checkOrInsertEvaluation :: FilePath -> Channel -> Out -> Handler ()
checkOrInsertEvaluation repoDir chan out = do
  test <- runDB $ get404 $ outTest out
  challenge <- runDB $ get404 $ testChallenge test
  maybeEvaluation <- runDB $ getBy $ UniqueEvaluationTestChecksum (outTest out) (outChecksum out)
  case maybeEvaluation of
    Just (Entity _ evaluation) -> do
      msg chan $ concat ["Already evaluated with score ", (T.pack $ fromMaybe "???" $ show <$> evaluationScore evaluation)]
    Nothing -> do
      msg chan $ "Start evaluation..."
      challengeDir <- getRepoDir $ challengePrivateRepo challenge
      resultOrException <- liftIO $ rawEval challengeDir (testMetric test) repoDir (testName test)
      case resultOrException of
        Right (Left _) -> do
          err chan "Cannot parse options, check the challenge repo"
        Right (Right (_, Just [result])) -> do
          msg chan $ concat [ "Evaluated! Score ", (T.pack $ show result) ]
          time <- liftIO getCurrentTime
          _ <- runDB $ insert $ Evaluation {
            evaluationTest=outTest out,
            evaluationChecksum=outChecksum out,
            evaluationScore=Just result,
            evaluationErrorMessage=Nothing,
            evaluationStamp=time }
          msg chan "Evaluation done"
        Right (Right (_, Just _)) -> do
          err chan "Unexpected multiple results (???)"
        Right (Right (_, Nothing)) -> do
          err chan "Error during the evaluation"
        Left exception -> do
          err chan $ "Evaluation failed: " ++ (T.pack $ show exception)

rawEval :: FilePath -> Metric -> FilePath -> Text -> IO (Either GEvalException (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe [MetricValue])))
rawEval challengeDir metric repoDir name = Import.try (runGEvalGetOptions [
                                                          "--metric", (show metric),
                                                          "--expected-directory", challengeDir,
                                                          "--out-directory", repoDir,
                                                          "--test-name", (T.unpack name)])

getSubmissionRepo :: Key Challenge -> RepoSpec -> Channel -> Handler (Maybe (Key Repo))
getSubmissionRepo challengeId repoSpec chan = do
  let url = repoSpecUrl repoSpec
  let branch = repoSpecBranch repoSpec
  maybeRepo <- runDB $ getBy $ UniqueUrlBranch url branch
  case maybeRepo of
    Just (Entity repoId repo) -> do
      msg chan "Repo already there"
      available <- checkRepoAvailibility challengeId repoId chan
      if available
         then
          do
           updateStatus <- updateRepo repoId chan
           if updateStatus
             then
               return $ Just repoId
             else
               return Nothing
         else
           return Nothing
    Nothing -> do
      challenge <- runDB $ get404 challengeId
      let repoId = challengePublicRepo challenge
      repo <- runDB $ get404 repoId
      repoDir <- getRepoDir repoId
      let repoCloningSpec = RepoCloningSpec {
        cloningSpecRepo = repoSpec,
        cloningSpecReferenceRepo = RepoSpec {
                repoSpecUrl = (T.pack repoDir),
                repoSpecBranch = (repoBranch repo)
                }
        }
      cloneRepo' repoCloningSpec chan

checkRepoAvailibility :: Key Challenge -> Key Repo -> Channel -> Handler Bool
checkRepoAvailibility challengeId repoId chan = do
  maybeOtherChallengeId <- runDB $ selectFirst ( [ChallengePublicRepo ==. repoId]
                                                 ||. [ChallengePrivateRepo ==. repoId]) []
  case maybeOtherChallengeId of
    Just _ -> do
      err chan "Repository already used as a challenge repo, please use a different repo or a different branch"
      return False
    Nothing -> do
      maybeOtherSubmissionId <- runDB $ selectFirst [SubmissionRepo ==. repoId,
                                                    SubmissionChallenge !=. challengeId] []
      case maybeOtherSubmissionId of
        Just _ -> do
          err chan "Repository already used as a submission repo for a different challenge, please use a different repo or a different branch"
          return False
        Nothing -> return True


challengeSubmissionWidget formWidget formEnctype challenge = $(widgetFile "challenge-submission")

submissionForm :: Maybe Text -> Maybe Text -> Maybe Text -> Form (Maybe Text, Maybe Text, Text, Text, Maybe Text)
submissionForm defaultUrl defaultBranch defaultGitAnnexRemote = renderBootstrap3 BootstrapBasicForm $ (,,,,)
    <$> aopt textField (fieldWithTooltip MsgSubmissionDescription MsgSubmissionDescriptionTooltip) Nothing
    <*> aopt textField (tagsfs MsgSubmissionTags) Nothing
    <*> areq textField (bfs MsgSubmissionUrl) defaultUrl
    <*> areq textField (bfs MsgSubmissionBranch) defaultBranch
    <*> aopt textField (bfs MsgSubmissionGitAnnexRemote) (Just defaultGitAnnexRemote)

getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR name = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) name

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR name = getChallengeSubmissions (\_ -> True) name

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition name = do
  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  (evaluationMaps, tests) <- getChallengeSubmissionInfos condition challengeId
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  challengeLayout True challenge (challengeAllSubmissionsWidget muserId challenge scheme challengeRepo evaluationMaps tests)

challengeAllSubmissionsWidget muserId challenge scheme challengeRepo submissions tests = $(widgetFile "challenge-all-submissions")

challengeLayout withHeader challenge widget = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  maybeUser <- maybeAuth
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
