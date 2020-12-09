module Handler.ShowChallenge where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)

import qualified Data.Text.Lazy as TL
import           Text.Markdown

import qualified Data.Text as T

import qualified Yesod.Table as Table

import Handler.Extract
import Handler.Shared
import Handler.Runner
import Handler.Tables
import Handler.TagUtils
import Handler.MakePublic
import Handler.Dashboard
import Handler.Common
import Handler.Evaluate

import qualified Data.ByteString as BS
import           Data.Word8 (isSpace, toLower)
import           Network.Wai (Request, requestHeaders)
import qualified Jose.Jwt as JWT
import qualified Jose.Jwa as JWA
import qualified Jose.Jwk as JWK

import Data.Maybe (fromJust)

import Text.Blaze

import Data.Aeson

import Gonito.ExtractMetadata (ExtractionOptions(..),
                               extractMetadataFromRepoDir,
                               GonitoMetadata(..),
                               parseTags,
                               Link(..))

import qualified Text.Read as TR

import GEval.Core
import GEval.EvaluationScheme

import PersistSHA1

import System.IO (readFile)

import Data.Text (pack, unpack)

import Data.List (nub)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

instance ToJSON LeaderboardEntry where
    toJSON entry = object
        [ "submitter" .= (formatSubmitter $ leaderboardUser entry)
        , "when" .= (submissionStamp $ leaderboardBestSubmission entry)
        , "version" .= (formatVersion $ leaderboardVersion entry)
        , "description" .= descriptionToBeShown (leaderboardBestSubmission entry)
                                                (leaderboardBestVariant entry)
                                                (leaderboardParams entry)
        , "times" .= leaderboardNumberOfSubmissions entry
        ]

getLeaderboardJsonR :: Text -> Handler Value
getLeaderboardJsonR name = do
  app <- getYesod
  let leaderboardStyle = appLeaderboardStyle $ appSettings app

  Entity challengeId _ <- runDB $ getBy404 $ UniqueName name
  (leaderboard, (_, tests)) <- getLeaderboardEntries 1 leaderboardStyle challengeId
  return $ array $ map (leaderboardEntryJson tests) leaderboard

leaderboardEntryJson tests entry = object [
  "metadata" .= entry,
  "metrics" .=
    map (\e@(Entity _ t) -> object [
            "metric" .= testName t,
            "score" .= (formatTruncatedScore (getTestFormattingOpts t) $ extractScoreFromLeaderboardEntry (getTestReference e) entry)]) tests]

getShowChallengeR :: Text -> Handler Html
getShowChallengeR name = do
  app <- getYesod
  let leaderboardStyle = appLeaderboardStyle $ appSettings app

  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName name
  Just repo <- runDB $ get $ challengePublicRepo challenge
  (leaderboard, (entries, tests)) <- getLeaderboardEntries 1 leaderboardStyle challengeId

  showAltLeaderboard <- runDB $ hasMetricsOfSecondPriority challengeId

  (altLeaderboard, altTests) <- if showAltLeaderboard
                               then
                                do
                                 (leaderboard', (_, tests')) <- getLeaderboardEntries 3 ByTag challengeId
                                 return $ (Just leaderboard', Just tests')
                               else
                                 return (Nothing, Nothing)

  mauth <- maybeAuth

  let params = getNumericalParams entries

  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  challengeLayout True challenge (showChallengeWidget mauth
                                                      challengeEnt
                                                      scheme
                                                      challengeRepo
                                                      repo
                                                      leaderboard
                                                      altLeaderboard
                                                      params
                                                      tests
                                                      altTests)

hasMetricsOfSecondPriority :: (PersistQueryRead backend, MonadIO m, BaseBackend backend ~ SqlBackend) => Key Challenge -> ReaderT backend m Bool
hasMetricsOfSecondPriority challengeId = do
  tests' <- selectList [TestChallenge ==. challengeId, TestActive ==. True] []
  let tests = filter (\t -> (evaluationSchemePriority $ testMetric $ entityVal t) == 2) tests'
  return $ not (null tests)


getChallengeReadmeR :: Text -> Handler Html
getChallengeReadmeR name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  readme <- challengeReadme name
  challengeLayout False challenge $ toWidget readme

challengeReadme :: Text -> HandlerFor App Html
challengeReadme name = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  theContents <- liftIO $ System.IO.readFile readmeFilePath
  return $ markdown def $ TL.pack theContents

showChallengeWidget :: Maybe (Entity User)
                      -> Entity Challenge
                      -> RepoScheme
                      -> Repo
                      -> Repo
                      -> [LeaderboardEntry]
                      -> (Maybe [LeaderboardEntry])
                      -> [Text]
                      -> [Entity Test]
                      -> (Maybe [Entity Test])
                      -> WidgetFor App ()
showChallengeWidget mUserEnt
                    (Entity challengeId challenge)
                    scheme
                    challengeRepo
                    repo
                    leaderboard
                    mAltLeaderboard
                    params
                    tests
                    mAltTests
  = $(widgetFile "show-challenge")
  where leaderboardWithRanks = zip [1..] leaderboard
        mAltLeaderboardWithRanks = zip [1..] <$> mAltLeaderboard
        maybeRepoLink = getRepoLink repo
        delta = Number 4
        higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests
        mUserId = entityKey <$> mUserEnt

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
      ukeys <- runDB $ selectList [PublicKeyUser ==. userId] []
      return $ not (null ukeys)
    Nothing -> return False
  challengeLayout False challenge (challengeHowTo
                                   challenge
                                   settings
                                   repo
                                   (idToBeShown challenge maybeUser)
                                   isIDSet
                                   isSSHUploaded
                                   (join $ userAltRepoScheme <$> entityVal <$> maybeUser)
                                   mToken)

idToBeShown :: p -> Maybe (Entity User) -> Text
idToBeShown _ maybeUser =
  case maybeUser of
   Just user ->  case userLocalId $ entityVal user of
                 Just localId -> localId
                 Nothing -> defaultIdToBe
   Nothing -> defaultIdToBe
  where defaultIdToBe = "YOURID" :: Text

defaultRepo :: RepoScheme -> Text -> Challenge -> Repo -> Maybe (Entity User) -> Text
defaultRepo SelfHosted repoHost challenge _ maybeUser = repoHost ++ (idToBeShown challenge maybeUser) ++ "/" ++ (challengeName challenge)
defaultRepo Branches _ _ repo _ = repoUrl repo

defaultBranch :: IsString a => RepoScheme -> Maybe a
defaultBranch SelfHosted = Just "master"
defaultBranch Branches = Nothing

challengeHowTo :: Challenge -> AppSettings -> Repo -> Text -> Bool -> Bool -> Maybe Text -> Maybe Text -> WidgetFor App ()
challengeHowTo challenge settings repo shownId isIDSet isSSHUploaded mAltRepoScheme mToken = $(widgetFile "challenge-how-to")
  where myBranch = "my-brilliant-branch" :: Text
        urlToYourRepo = case mAltRepoScheme of
          Just altRepoScheme -> encodeSlash (altRepoScheme <> (challengeName challenge))
          Nothing -> "URL_TO_YOUR_REPO"

postArchiveR :: ChallengeId -> Handler Html
postArchiveR challengeId = doSetArchive True challengeId

postUnarchiveR :: ChallengeId -> Handler Html
postUnarchiveR challengeId = doSetArchive False challengeId

doSetArchive :: Bool -> ChallengeId -> Handler Html
doSetArchive status challengeId = do
  runDB $ update challengeId [ChallengeArchived =. Just status]
  challenge <- runDB $ get404 challengeId
  getShowChallengeR $ challengeName challenge


archiveForm :: ChallengeId -> Form ChallengeId
archiveForm challengeId = renderBootstrap3 BootstrapBasicForm $ areq hiddenField "" (Just challengeId)

getChallengeSubmissionR :: Text -> Handler Html
getChallengeSubmissionR name = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName name
   maybeUser <- maybeAuth

   Just repo <- runDB $ get $ challengePublicRepo challenge
   app <- getYesod
   let scheme = appRepoScheme $ appSettings app
   let repoHost = appRepoHost $ appSettings app

   let defaultUrl = fromMaybe (defaultRepo scheme repoHost challenge repo maybeUser)
                              ((<> name) <$> (join $ userAltRepoScheme <$> entityVal <$> maybeUser))

   (formWidget, formEnctype) <- generateFormPost $ submissionForm (Just defaultUrl) (defaultBranch scheme) (repoGitAnnexRemote repo)
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR name = do
    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName name
    ((result, _), _) <- runFormPost $ submissionForm Nothing Nothing Nothing
    let submissionData' = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just submissionData = submissionData'

    userId <- requireAuthId
    runViewProgress $ doCreateSubmission userId challengeId submissionData

postTriggerLocallyR :: Handler TypedContent
postTriggerLocallyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just localId) <- lookupPostParam "user"
  mBranch <- lookupPostParam "branch"
  mGitAnnexRemote <- lookupPostParam "git-annex-remote"
  [Entity userId _] <- runDB $ selectList [UserLocalId ==. Just localId] []

  app <- getYesod
  let repoHost = appRepoHost $ appSettings app

  let localRepo = repoHost ++ localId ++ "/" ++ challengeName
  trigger userId challengeName localRepo mBranch mGitAnnexRemote

postTriggerRemotelyR :: Handler TypedContent
postTriggerRemotelyR = do
  (Just challengeName) <- lookupPostParam "challenge"
  (Just url) <- lookupPostParam "url"
  (Just token) <- lookupPostParam "token"
  mBranch <- lookupPostParam "branch"
  mGitAnnexRemote <- lookupPostParam "git-annex-remote"
  doTrigger token challengeName url mBranch mGitAnnexRemote

postTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
postTriggerRemotelySimpleR token challengeName url branch =
  doTrigger token challengeName (decodeSlash url) (Just branch) Nothing

getTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
getTriggerRemotelySimpleR token challengeName url branch =
  doTrigger token challengeName (decodeSlash url) (Just branch) Nothing

data GitServerPayload = GitServerPayload {
  gitServerPayloadRef :: Text,
  -- Unfortunately, the URL is given in "ssh_url" field
  -- for Gogs and "git_ssh_url" for GitLab, hence two
  -- fields here
  gitServerPayloadSshUrl :: Maybe Text,
  gitServerPayloadGitSshUrl :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON GitServerPayload where
    parseJSON (Object o) = GitServerPayload
      <$> o .: "ref"
      <*> ((o .: "repository") >>= (.:? "ssh_url"))
      <*> ((o .: "repository") >>= (.:? "git_ssh_url"))

postTriggerByWebhookR :: Text -> Text -> Handler TypedContent
postTriggerByWebhookR token challengeName = do
  payload <- requireJsonBody :: Handler GitServerPayload
  let ref = gitServerPayloadRef payload
  let refPrefix = "refs/heads/"
  if refPrefix `isPrefixOf` ref
    then
      do
        let branch = T.replace refPrefix "" ref
        let url = fromMaybe (fromJust $ gitServerPayloadGitSshUrl payload)
                            (gitServerPayloadSshUrl payload)
        doTrigger token challengeName url (Just branch) Nothing
    else
      error $ "unexpected ref `" ++ (T.unpack ref) ++ "`"


doTrigger :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
doTrigger token challengeName url mBranch mGitAnnexRemote = do
  [Entity userId _] <- runDB $ selectList [UserTriggerToken ==. Just token] []
  trigger userId challengeName url mBranch mGitAnnexRemote

trigger :: UserId -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
trigger userId challengeName url mBranch mGitAnnexRemote = do
  let branch = fromMaybe "master" mBranch
  mChallengeEnt <- runDB $ getBy $ UniqueName challengeName

  let defSubmission = ChallengeSubmissionData {
        challengeSubmissionDataDescription = Nothing,
        challengeSubmissionDataTags = Nothing,
        challengeSubmissionDataRepo = RepoSpec {
            repoSpecUrl=url,
            repoSpecBranch=branch,
            repoSpecGitAnnexRemote=mGitAnnexRemote}
        }

  case mChallengeEnt of
    Just (Entity challengeId _) -> runOpenViewProgress $ doCreateSubmission userId challengeId defSubmission
    Nothing -> return $ toTypedContent (("Unknown challenge `" ++ (Data.Text.unpack challengeName) ++ "`. Cannot be triggered, must be submitted manually at Gonito.net!\n") :: String)

isBefore :: UTCTime -> Maybe UTCTime -> Bool
isBefore _ Nothing = True
isBefore moment (Just deadline) = moment <= deadline

-- | An attempt to filtre out mistaken/unwanted submissions (without cloning
-- the submission repo, just by looking at the metadata)
willClone :: Challenge -> ChallengeSubmissionData -> Bool
willClone challenge submissionData =
  (challengeName challenge) `isInfixOf` url && branch /= dontPeek && not (dontPeek `isInfixOf` url)
  where url = repoSpecUrl $ challengeSubmissionDataRepo submissionData
        branch = repoSpecBranch $ challengeSubmissionDataRepo submissionData
        dontPeek = "dont-peek"


-- | Main place where submission is done (whether manually or by trigger)
doCreateSubmission :: UserId -> Key Challenge -> ChallengeSubmissionData -> Channel -> Handler ()
doCreateSubmission userId challengeId challengeSubmissionData chan = do
  challenge <- runDB $ get404 challengeId

  version <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  theNow <- liftIO getCurrentTime

  if theNow `isBefore` (versionDeadline $ entityVal version)
    then
     do
      let wanted = willClone challenge challengeSubmissionData
      if wanted
        then
          doCreateSubmission' (challengeArchived challenge) userId challengeId challengeSubmissionData chan
        else
          msg chan "Refusing to clone the submission from this URL"
    else
      msg chan "Submission is past the deadline, no submission will be accepted from now on."

doCreateSubmission' :: Maybe Bool -> UserId -> Key Challenge -> ChallengeSubmissionData -> Channel -> Handler ()
doCreateSubmission' (Just True) _ _ _ chan = msg chan "This challenge is archived, you cannot submit to it. Ask the site admin to unarchive it."
doCreateSubmission' _ userId challengeId challengeSubmissionData chan = do
  let mDescription = challengeSubmissionDataDescription challengeSubmissionData
  let mTags = challengeSubmissionDataTags challengeSubmissionData
  let repoSpec = challengeSubmissionDataRepo challengeSubmissionData

  maybeRepoKey <- getSubmissionRepo userId challengeId repoSpec chan
  case maybeRepoKey of
    Just repoId -> do

      challenge <- runDB $ get404 challengeId
      user <- runDB $ get404 userId

      relevantIndicators <- getOngoingTargets challengeId

      (Entity mainTestId mainTest) <- runDB $ fetchMainTest challengeId

      (Entity _ currentVersion) <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
      let submittedMajorVersion = versionMajor currentVersion

      let orderDirection = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
            TheHigherTheBetter -> E.desc
            TheLowerTheBetter -> E.asc

      bestResultSoFar <- runDB $ E.select $ E.from $ \(evaluation, submission, variant, out, test, version) -> do
              E.where_ (submission ^. SubmissionChallenge E.==. E.val challengeId
                        E.&&. submission ^. SubmissionIsHidden E.==. E.val False
                        E.&&. variant ^. VariantSubmission E.==. submission ^. SubmissionId
                        E.&&. evaluation ^. EvaluationChecksum E.==. out ^. OutChecksum
                        E.&&. (E.not_ (E.isNothing (evaluation ^. EvaluationScore)))
                        E.&&. out ^. OutVariant E.==. variant ^. VariantId
                        E.&&. evaluation ^. EvaluationTest E.==. test ^. TestId
                        E.&&. test ^. TestChallenge E.==. E.val challengeId
                        E.&&. test ^. TestName E.==. E.val (testName mainTest)
                        E.&&. test ^. TestMetric E.==. E.val (testMetric mainTest)
                        E.&&. test ^. TestActive
                        E.&&. (evaluation ^. EvaluationVersion E.==. E.just (version ^. VersionCommit)
                               E.||. E.isNothing (evaluation ^. EvaluationVersion))
                        E.&&. version ^. VersionCommit E.==. test ^. TestCommit
                        E.&&. version ^. VersionMajor E.>=. E.val submittedMajorVersion)
              E.orderBy [orderDirection (evaluation ^. EvaluationScore)]
              E.limit 1
              return evaluation
      let bestScoreSoFar = join (evaluationScore <$> entityVal <$> (listToMaybe bestResultSoFar))

      case bestScoreSoFar of
        Just s -> msg chan ("best score so far is: " ++ (Data.Text.pack $ show s))
        Nothing -> msg chan "first submission so far"

      repo <- runDB $ get404 repoId

      repoDir <- getRepoDirOrClone repoId chan

      gonitoMetadata <- liftIO
                       $ extractMetadataFromRepoDir repoDir (ExtractionOptions {
                                                                extractionOptionsDescription = mDescription,
                                                                extractionOptionsTags = Just $ parseTags mTags,
                                                                extractionOptionsGeneralParams = Nothing,
                                                                extractionOptionsUnwantedParams = Nothing,
                                                                extractionOptionsParamFiles = Nothing,
                                                                extractionOptionsMLRunPath = Nothing,
                                                                extractionOptionsExternalLinks = Nothing,
                                                                extractionOptionsDependencies = Nothing })

      submissionId <- getSubmission userId
                                   repoId
                                   (repoCurrentCommit repo)
                                   challengeId
                                   (gonitoMetadataDescription gonitoMetadata)
                                   chan

      _ <- runDB $ mapM insert $ map (\l -> ExternalLink {
                                        externalLinkSubmission = submissionId,
                                        externalLinkTitle = linkTitle l,
                                        externalLinkUrl = linkUrl l }) $ gonitoMetadataExternalLinks gonitoMetadata

      _ <- runDB $ mapM insertUnique $ map (\s -> Dependency {
                                                  dependencySubRepoCommit = s,
                                                  dependencySuperRepoCommit = (repoCurrentCommit repo) }) $ gonitoMetadataDependencies gonitoMetadata

      outs <- getOuts False chan submissionId (gonitoMetadataGeneralParams gonitoMetadata)

      currentTagIds <- runDB $ selectList [SubmissionTagSubmission ==. submissionId] []

      runDB $ addTags submissionId (gonitoMetadataTags gonitoMetadata)  (
        map (submissionTagTag . entityVal) currentTagIds)
      msg chan "SUBMISSION CREATED"

      app <- getYesod

      newScores <- mapM (getScoreForOut mainTestId) outs
      let newScores' = catMaybes newScores
      let newScores'' = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
            TheHigherTheBetter -> reverse $ sort newScores'
            TheLowerTheBetter -> sort newScores'
      let compOp = case getMetricOrdering (evaluationSchemeMetric $ testMetric mainTest) of
            TheLowerTheBetter -> (<)
            TheHigherTheBetter -> (>)

      let submissionLink = slackLink app "submission" ("q/" ++ (fromSHA1ToText (repoCurrentCommit repo)))

      case bestScoreSoFar of
        Just b ->  case newScores'' of
                    (s:_) -> if compOp s b
                             then
                              do
                                let challengeLink = slackLink app (challengeTitle challenge) ("challenge/"
                                                                                              ++ (challengeName challenge))
                                let message = ("Whoa! New best result for "
                                               ++ challengeLink
                                               ++ " challenge by "
                                               ++ (fromMaybe "???" $ userName user)
                                               ++ ", "
                                               ++ (T.pack $ show $ testMetric mainTest)
                                               ++ ": "
                                               ++ (formatScore (testPrecision mainTest) s)
                                               ++ " ("
                                               ++ (if s > b
                                                   then "+"
                                                   else "")
                                               ++ (formatScore (testPrecision mainTest) (s-b))
                                               ++ ")."
                                               ++ " See " ++ submissionLink ++ "."
                                               ++ " :clap:")
                                msg chan message
                                case appNewBestResultSlackHook $ appSettings app of
                                  Just "" -> return ()
                                  Just hook -> liftIO $ runSlackHook hook message

                                  Nothing -> return ()
                             else return ()
                    [] -> return ()
        Nothing -> return ()

      if appAutoOpening $ appSettings app
        then
          doMakePublic userId submissionId chan
        else
          return ()

      if not (null relevantIndicators)
        then
          checkIndicators user challengeId submissionId submissionLink relevantIndicators chan
        else
          return ()

    Nothing -> return ()

checkIndicators :: User -> ChallengeId -> SubmissionId -> Text -> [IndicatorEntry] -> Channel -> Handler ()
checkIndicators user challengeId submissionId submissionLink relevantIndicators chan = do
  msg chan "Checking indicators..."
  theNow <- liftIO $ getCurrentTime
  mapM_ (\indicator -> checkIndicator theNow user challengeId submissionId submissionLink indicator chan) relevantIndicators

checkIndicator :: UTCTime -> User -> ChallengeId -> SubmissionId -> Text -> IndicatorEntry -> Channel -> Handler ()
checkIndicator theNow user challengeId submissionId submissionLink indicator chan = do
  (entries, _) <- runDB $ getChallengeSubmissionInfos 1 (\(Entity sid _) -> sid == submissionId) (const True) id challengeId
  mapM_ (\t -> checkTarget theNow user submissionLink entries indicator t chan) (indicatorEntryTargets indicator)

checkTarget :: UTCTime -> User -> Text -> [TableEntry] -> IndicatorEntry -> Entity Target -> Channel -> Handler ()
checkTarget theNow user submissionLink entries indicator target chan = do
  app <- getYesod
  let status = getTargetStatus theNow entries indicator target
  if status == TargetPassed
    then
     do
      let message = "Congratulations!!! The target " ++ indicatorText
                     ++ " was beaten by "
                     ++ (fromMaybe "???" $ userName user)
                     ++ ", "
                     ++ " See " ++ submissionLink ++ "."
                     ++ (T.replicate 10 " :champagne: ") ++ " :mleczko: "
      msg chan message
      case appNewBestResultSlackHook $ appSettings app of
          Just "" -> return ()
          Just hook -> liftIO $ runSlackHook hook message
          Nothing -> return ()
    else
       return ()
  where indicatorText = prettyIndicatorEntry indicator

getScoreForOut :: (PersistQueryRead (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => Key Test -> Out -> HandlerFor site (Maybe Double)
getScoreForOut mainTestId out = do
  mEvaluation <- runDB $ selectFirst [EvaluationChecksum ==. (outChecksum out),
                                     EvaluationTest ==. mainTestId]
                                    []
  return $ case mEvaluation of
             Just evaluation -> evaluationScore $ entityVal evaluation
             Nothing -> Nothing

getSubmission :: UserId -> Key Repo -> SHA1 -> Key Challenge -> Text -> Channel -> Handler (Key Submission)
getSubmission userId repoId commit challengeId description chan = do
  challenge <- runDB $ get404 challengeId
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
        submissionIsPublic=False,
        submissionIsHidden=False,
        submissionVersion=challengeVersion challenge}

getSubmissionRepo :: UserId -> Key Challenge -> RepoSpec -> Channel -> Handler (Maybe (Key Repo))
getSubmissionRepo userId challengeId repoSpec chan = getPossiblyExistingRepo checkRepoAvailibility userId challengeId repoSpec chan

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

challengeSubmissionWidget :: (ToMarkup a1, ToWidget App a2) => a2 -> a1 -> Challenge -> WidgetFor App ()
challengeSubmissionWidget formWidget formEnctype challenge = $(widgetFile "challenge-submission")

data ChallengeSubmissionData = ChallengeSubmissionData {
  challengeSubmissionDataDescription :: Maybe Text,
  challengeSubmissionDataTags :: Maybe Text,
  challengeSubmissionDataRepo :: RepoSpec }


submissionForm :: Maybe Text -> Maybe Text -> Maybe Text -> Form ChallengeSubmissionData
submissionForm defaultUrl defBranch defaultGitAnnexRemote = renderBootstrap3 BootstrapBasicForm $ ChallengeSubmissionData
    <$> aopt textField (fieldWithTooltip MsgSubmissionDescription MsgSubmissionDescriptionTooltip) Nothing
    <*> aopt textField (tagsfs MsgSubmissionTags) Nothing
    <*> (RepoSpec <$> areq textField (bfs MsgSubmissionUrl) defaultUrl
                  <*> areq textField (bfs MsgSubmissionBranch) defBranch
                  <*> aopt textField (bfs MsgSubmissionGitAnnexRemote) (Just defaultGitAnnexRemote))

getChallengeMySubmissionsJsonR :: Text -> Handler Value
getChallengeMySubmissionsJsonR name = do
  req <- waiRequest
  let mToken = case lookup "Authorization" (Network.Wai.requestHeaders req) of
                 Nothing -> Nothing
                 Just authHead -> case BS.break isSpace authHead of
                   (strategy, token)
                     | BS.map Data.Word8.toLower strategy == "bearer" -> (Just $ BS.filter (/= 32) token)
                     | otherwise -> Nothing
  mUserEnt <- maybeAuth

  app <- getYesod
  let jwk = fromJust $ appJSONWebKey $ appSettings app

  dtoken <- liftIO $ JWT.decode [jwk] (Just (JWT.JwsEncoding JWA.RS256)) $ fromJust mToken
  return $ array [show dtoken]

getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR name = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) name

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR name = getChallengeSubmissions (\_ -> True) name

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition name = do
  Entity challengeId challenge <- runDB $ getBy404 $ UniqueName name
  (evaluationMaps, tests') <- runDB $ getChallengeSubmissionInfos 1 condition (const True) id challengeId
  let tests = sortBy testComparator tests'
  mauth <- maybeAuth
  let muserId = (\(Entity uid _) -> uid) <$> mauth

  app <- getYesod
  let scheme = appRepoScheme $ appSettings app

  challengeRepo <- runDB $ get404 $ challengePublicRepo challenge

  let params = getNumericalParams evaluationMaps

  challengeLayout True challenge (challengeAllSubmissionsWidget muserId
                                                                challenge
                                                                scheme
                                                                challengeRepo
                                                                evaluationMaps
                                                                tests
                                                                params)

getNumericalParams :: [TableEntry] -> [Text]
getNumericalParams entries = filter (isNumericalParam entries) $ getAllParams entries

isNumericalParam :: [TableEntry] -> Text -> Bool
isNumericalParam entries param =
  all doesTextRepresentNumber
  $ concat
  $ map ((map parameterValue)
       . (filter (\p -> parameterName p == param))
       . (map entityVal)
       . tableEntryParams) entries

doesTextRepresentNumber :: Text -> Bool
doesTextRepresentNumber t = isJust $ ((TR.readMaybe $ T.unpack t) :: Maybe Double)

getAllParams :: [TableEntry] -> [Text]
getAllParams entries = sort
               $ nub
               $ concat
               $ map (\entry -> map (parameterName . entityVal) (tableEntryParams entry)) entries


challengeAllSubmissionsWidget :: Maybe UserId
                                -> Challenge
                                -> RepoScheme
                                -> Repo
                                -> [TableEntry]
                                -> [Entity Test]
                                -> [Text]
                                -> WidgetFor App ()
challengeAllSubmissionsWidget muserId challenge scheme challengeRepo submissions tests params =
  $(widgetFile "challenge-all-submissions")
  where delta = Number 4
        higherTheBetterArray = getIsHigherTheBetterArray $ map entityVal tests

paramGraphsWidget :: Challenge -> [Entity Test] -> [Text] -> WidgetFor App ()
paramGraphsWidget challenge tests params = $(widgetFile "param-graphs")
  where chartJSs = getChartJss challenge selectedTests params
        selectedTests = reverse $ getMainTests tests

getChartJss :: Challenge -> [Entity Test] -> [Text] -> JavascriptUrl (Route App)
getChartJss challenge tests params =
  mconcat $ [(getChartJs challenge test param) | test <- tests, param <- params]

getChartJs :: Challenge
             -> Entity Test
             -> Text
             -> JavascriptUrl (Route App)
getChartJs challenge (Entity testId test) param = [julius|
$.getJSON("@{ChallengeParamGraphDataR (challengeName challenge) testId param}", function(data) {
        c3.generate({
                bindto: '#chart-' + #{toJSON param} + '-' + #{toJSON testId},
                data: data,
                axis: {
                   x: {
                     label: #{toJSON param},
                   },
                   y: {
                     label: #{toJSON testFormatted},
                   }
                }
    }) });
|]
   where testFormatted = formatTest test


challengeLayout :: Bool -> Challenge -> WidgetFor App () -> HandlerFor App Html
challengeLayout withHeader challenge widget = do
  tagsAvailableAsJSON <- runDB $ getAvailableTagsAsJSON
  version <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  let versionFormatted = formatVersion ((versionMajor $ entityVal version),
                                        (versionMinor $ entityVal version),
                                        (versionPatch $ entityVal version))
  maybeUser <- maybeAuth
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
