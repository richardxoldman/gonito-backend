module Handler.ShowChallenge where

import Import hiding (Proxy, fromList)
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

import Database.Persist.Sql (fromSqlKey)

import qualified Data.Map as Map

import qualified Data.ByteString as BS
import           Data.Word8 (isSpace, toLower)
import           Network.Wai (requestHeaders)
import qualified Jose.Jwt as JWT
import qualified Jose.Jwa as JWA


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

import Data.Swagger hiding (get)
import qualified Data.Swagger as DS

import Data.Swagger.Declare
import Control.Lens hiding ((.=), (^.))
import Data.Proxy as DPR
import Data.HashMap.Strict.InsOrd (fromList)

instance ToJSON LeaderboardEntry where
    toJSON entry = object
        [ "submitter" .= (formatSubmitter $ leaderboardUser entry)
        , "when" .= (submissionStamp $ leaderboardBestSubmission entry)
        , "version" .= (formatVersion $ leaderboardVersion entry)
        , "description" .= descriptionToBeShown (leaderboardBestSubmission entry)
                                                (leaderboardBestVariant entry)
                                                (leaderboardParams entry)
        , "times" .= leaderboardNumberOfSubmissions entry
        , "hash" .= (fromSHA1ToText $ submissionCommit $ leaderboardBestSubmission entry)
        ]

instance ToSchema LeaderboardEntry where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    intSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Int)
    return $ NamedSchema (Just "LeaderboardEntry") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("submitter", stringSchema)
                     , ("when", stringSchema)
                     , ("version", stringSchema)
                     , ("description", stringSchema)
                     , ("times", intSchema)
                     , ("hash", stringSchema)
                    ]
        & required .~ [ "submitter", "when", "version", "description", "times", "hash" ]


declareLeaderboardSwagger :: Declare (Definitions Schema) Swagger
declareLeaderboardSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  leaderboardResponse      <- declareResponse (Proxy :: Proxy [LeaderboardEntry])

  return $ mempty
    & paths .~
        fromList [ ("/api/leaderboard/{challengeName}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ "Returns a leaderboard for a given challenge"
                                        & at 200 ?~ Inline leaderboardResponse))
                 ]


leaderboardApi :: Swagger
leaderboardApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareLeaderboardSwagger mempty



getLeaderboardJsonR :: Text -> Handler Value
getLeaderboardJsonR challengeName = do
  app <- getYesod
  let leaderboardStyle = appLeaderboardStyle $ appSettings app

  Entity challengeId _ <- runDB $ getBy404 $ UniqueName challengeName
  (leaderboard, (_, tests)) <- getLeaderboardEntries 1 leaderboardStyle challengeId
  return $ array $ map (leaderboardEntryJson tests) leaderboard

leaderboardEntryJson :: (ToJSON (f Value), Functor f) => f (Entity Test) -> LeaderboardEntry -> Value
leaderboardEntryJson tests entry = object [
  "metadata" .= entry,
  "metrics" .=
    map (\e@(Entity _ t) -> object [
            "metric" .= testName t,
            "score" .= (formatTruncatedScore (getTestFormattingOpts t) $ extractScoreFromLeaderboardEntry (getTestReference e) entry)]) tests]

getShowChallengeR :: Text -> Handler Html
getShowChallengeR challengeName = do
  app <- getYesod
  let leaderboardStyle = appLeaderboardStyle $ appSettings app

  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName challengeName
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
getChallengeReadmeR challengeName = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName
  readme <- challengeReadme challengeName
  challengeLayout False challenge $ toWidget readme

challengeReadmeInMarkdownApi :: Swagger
challengeReadmeInMarkdownApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareChallengeReadmeInMarkdownSwagger mempty

declareChallengeReadmeInMarkdownSwagger :: Declare (Definitions Schema) Swagger
declareChallengeReadmeInMarkdownSwagger = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  return $ mempty
    & paths .~
        fromList [ ("/api/challenge-readme/{challengeName}/markdown",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/text"]
                                        & description ?~ "Returns the challenge README in Markdown"))
                 ]

getChallengeReadmeInMarkdownR :: Text -> Handler TL.Text
getChallengeReadmeInMarkdownR challengeName = doChallengeReadmeContents challengeName

challengeReadme :: Text -> Handler Html
challengeReadme challengeName = do
  theContents <- doChallengeReadmeContents challengeName
  return $ markdown def theContents

doChallengeReadmeContents :: Text -> Handler TL.Text
doChallengeReadmeContents challengeName = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName
  let repoId = challengePublicRepo challenge
  repoDir <- getRepoDir repoId
  let readmeFilePath = repoDir </> readmeFile
  theContents <- liftIO $ System.IO.readFile readmeFilePath
  return $ TL.pack theContents

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
  | sitePrefix `isPrefixOf` theUrl = Just $ (browsableGitRepo bareRepoName) ++ "/" ++ (repoBranch repo)
  | otherwise = Nothing
  where sitePrefix = "git://gonito.net/" :: Text
        sitePrefixLen = length sitePrefix
        theUrl = repoUrl repo
        bareRepoName = drop sitePrefixLen theUrl

getChallengeHowToR :: Text -> Handler Html
getChallengeHowToR challengeName = do
  (Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName
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
getChallengeSubmissionR challengeName = do
   (Entity _ challenge) <- runDB $ getBy404 $ UniqueName challengeName
   maybeUser <- maybeAuth

   Just repo <- runDB $ get $ challengePublicRepo challenge
   app <- getYesod
   let scheme = appRepoScheme $ appSettings app
   let repoHost = appRepoHost $ appSettings app

   let defaultUrl = fromMaybe (defaultRepo scheme repoHost challenge repo maybeUser)
                              ((<> challengeName) <$> (join $ userAltRepoScheme <$> entityVal <$> maybeUser))

   (formWidget, formEnctype) <- generateFormPost $ submissionForm (Just defaultUrl) (defaultBranch scheme) (repoGitAnnexRemote repo)
   challengeLayout True challenge $ challengeSubmissionWidget formWidget formEnctype challenge

postChallengeSubmissionJsonR :: Text -> Handler Value
postChallengeSubmissionJsonR challengeName = do
    Entity userId _ <- requireAuthPossiblyByToken

    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName
    ((result, _), _) <- runFormPost $ submissionForm Nothing Nothing Nothing
    let submissionData' = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just submissionData = submissionData'

    runViewProgressAsynchronously $ doCreateSubmission userId challengeId submissionData

postChallengeSubmissionR :: Text -> Handler TypedContent
postChallengeSubmissionR challengeName = do
    userId <- requireAuthId

    (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName
    ((result, _), _) <- runFormPost $ submissionForm Nothing Nothing Nothing
    let submissionData' = case result of
          FormSuccess res -> Just res
          _ -> Nothing
        Just submissionData = submissionData'

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
  (Just theUrl) <- lookupPostParam "url"
  (Just token) <- lookupPostParam "token"
  mBranch <- lookupPostParam "branch"
  mGitAnnexRemote <- lookupPostParam "git-annex-remote"
  doTrigger token challengeName theUrl mBranch mGitAnnexRemote

postTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
postTriggerRemotelySimpleR token challengeName theUrl branch =
  doTrigger token challengeName (decodeSlash theUrl) (Just branch) Nothing

getTriggerRemotelySimpleR :: Text -> Text -> Text -> Text -> Handler TypedContent
getTriggerRemotelySimpleR token challengeName theUrl branch =
  doTrigger token challengeName (decodeSlash theUrl) (Just branch) Nothing

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
        let theUrl = fromMaybe (fromJust $ gitServerPayloadGitSshUrl payload)
                            (gitServerPayloadSshUrl payload)
        doTrigger token challengeName theUrl (Just branch) Nothing
    else
      error $ "unexpected ref `" ++ (T.unpack ref) ++ "`"


doTrigger :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
doTrigger token challengeName theUrl mBranch mGitAnnexRemote = do
  [Entity userId _] <- runDB $ selectList [UserTriggerToken ==. Just token] []
  trigger userId challengeName theUrl mBranch mGitAnnexRemote

trigger :: UserId -> Text -> Text -> Maybe Text -> Maybe Text -> Handler TypedContent
trigger userId challengeName theUrl mBranch mGitAnnexRemote = do
  let branch = fromMaybe "master" mBranch
  mChallengeEnt <- runDB $ getBy $ UniqueName challengeName

  let defSubmission = ChallengeSubmissionData {
        challengeSubmissionDataDescription = Nothing,
        challengeSubmissionDataTags = Nothing,
        challengeSubmissionDataRepo = RepoSpec {
            repoSpecUrl=theUrl,
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
  (challengeName challenge) `isInfixOf` theUrl && branch /= dontPeek && not (dontPeek `isInfixOf` theUrl)
  where theUrl = repoSpecUrl $ challengeSubmissionDataRepo submissionData
        branch = repoSpecBranch $ challengeSubmissionDataRepo submissionData
        dontPeek = "dont-peek"


-- | Main place where submission is done (whether manually or by trigger)
doCreateSubmission :: UserId -> Key Challenge -> ChallengeSubmissionData -> Channel -> Handler ()
doCreateSubmission userId challengeId challengeSubmissionData chan = do
  challenge <- runDB $ get404 challengeId

  theVersion <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  theNow <- liftIO getCurrentTime

  if theNow `isBefore` (versionDeadline $ entityVal theVersion)
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

      bestResultSoFar <- runDB $ E.select $ E.from $ \(evaluation, submission, variant, out, test, theVersion) -> do
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
                        E.&&. (evaluation ^. EvaluationVersion E.==. E.just (theVersion ^. VersionCommit)
                               E.||. E.isNothing (evaluation ^. EvaluationVersion))
                        E.&&. theVersion ^. VersionCommit E.==. test ^. TestCommit
                        E.&&. theVersion ^. VersionMajor E.>=. E.val submittedMajorVersion)
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
getSubmission userId repoId commit challengeId subDescription chan = do
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
        submissionDescription=subDescription,
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

data JwtAuthInfo = JwtAuthInfo Text
  deriving (Show, Eq)

instance FromJSON JwtAuthInfo where
 parseJSON (Object v) =
    JwtAuthInfo <$> v .: "preferred_username"
 parseJSON _ = mzero

jwtAuthInfoIdent :: JwtAuthInfo -> Text
jwtAuthInfoIdent (JwtAuthInfo ident) = ident

authorizationTokenAuth :: Handler (Maybe JwtAuthInfo)
authorizationTokenAuth = do
  app <- getYesod
  let mJwk = appJSONWebKey $ appSettings app

  case mJwk of
    Just jwk -> do
      req <- waiRequest
      case lookup "Authorization" (Network.Wai.requestHeaders req) of
        Nothing -> return Nothing
        Just authHead -> case BS.break isSpace authHead of
           (strategy, token')
             | BS.map Data.Word8.toLower strategy == "bearer" -> do
                let token = BS.filter (/= 32) token'
                einfo <- liftIO $ JWT.decode [jwk] (Just (JWT.JwsEncoding JWA.RS256)) token
                return $ case einfo of
                           Right (JWT.Jws (_, infos)) -> decode $ fromStrict infos
                           _ -> Nothing
             | otherwise -> return Nothing
    Nothing -> return Nothing

maybeAuthPossiblyByToken :: Handler (Maybe (Entity User))
maybeAuthPossiblyByToken = do
  mInfo <- authorizationTokenAuth
  case mInfo of
    Just infos -> do
      x <- runDB $ getBy $ UniqueUser $ jwtAuthInfoIdent infos
      case x of
        Just entUser -> return $ Just entUser
        Nothing -> maybeAuth
    Nothing -> maybeAuth


requireAuthPossiblyByToken :: Handler (Entity User)
requireAuthPossiblyByToken = do
  mInfo <- authorizationTokenAuth
  case mInfo of
    Just infos -> do
      x <- runDB $ getBy $ UniqueUser $ jwtAuthInfoIdent infos
      case x of
        Just entUser -> return entUser
        Nothing -> requireAuth
    Nothing -> requireAuth

getUserInfoR :: Handler Value
getUserInfoR = do
  (Entity _ user) <- requireAuthPossiblyByToken
  return $ String $ userIdent user

getAddUserR :: Handler Value
getAddUserR = do
  mInfo <- authorizationTokenAuth
  case mInfo of
    Just infos -> do
      let ident = jwtAuthInfoIdent infos
      x <- runDB $ getBy $ UniqueUser ident
      case x of
        Just _ -> return $ Bool False
        Nothing -> do
          _ <- runDB $ insert User
            { userIdent = ident
            , userPassword = Nothing
            , userName = Nothing
            , userIsAdmin = False
            , userLocalId = Nothing
            , userIsAnonymous = False
            , userAvatar = Nothing
            , userVerificationKey = Nothing
            , userKeyExpirationDate = Nothing
            , userTriggerToken = Nothing
            , userAltRepoScheme = Nothing
            }
          return $ Bool True
    Nothing -> return $ Bool False

declareAllSubmissionsApi :: String -> String -> Declare (Definitions Schema) Swagger
declareAllSubmissionsApi q d = do
  -- param schemas
  let challengeNameSchema = toParamSchema (Proxy :: Proxy String)

  allSubmissionsResponse <- declareResponse (Proxy :: Proxy SubmissionsView)

  return $ mempty
    & paths .~
        fromList [ ("/api/" ++ q ++ "/{challengeName}",
                    mempty & DS.get ?~ (mempty
                                        & parameters .~ [ Inline $ mempty
                                                          & name .~ "challengeName"
                                                          & required ?~ True
                                                          & schema .~ ParamOther (mempty
                                                                                  & in_ .~ ParamPath
                                                                                  & paramSchema .~ challengeNameSchema) ]
                                        & produces ?~ MimeList ["application/json"]
                                        & description ?~ T.pack d
                                        & at 200 ?~ Inline allSubmissionsResponse))
                 ]


allSubmissionsApi :: Swagger
allSubmissionsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareAllSubmissionsApi "challenge-all-submissions" "Returns all submissions for a challenge") mempty

mySubmissionsApi :: Swagger
mySubmissionsApi = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareAllSubmissionsApi "challenge-my-submissions" "Returns all submissions for a challenge for the user") mempty

getChallengeAllSubmissionsJsonR :: Text -> Handler Value
getChallengeAllSubmissionsJsonR challengeName = do
  v <- fetchAllSubmissionsView challengeName
  return $ toJSON v

getChallengeMySubmissionsJsonR :: Text -> Handler Value
getChallengeMySubmissionsJsonR challengeName = do
  v <- fetchMySubmissionsView challengeName
  return $ toJSON v

fetchAllSubmissionsView :: Text -> Handler SubmissionsView
fetchAllSubmissionsView challengeName = do
  fetchChallengeSubmissionsView (const True) challengeName

fetchMySubmissionsView :: Text -> Handler SubmissionsView
fetchMySubmissionsView challengeName = do
  Entity userId _ <- requireAuthPossiblyByToken
  fetchChallengeSubmissionsView (\(Entity _ submission) -> (submissionSubmitter submission == userId)) challengeName

convertTagInfoToView :: (Entity Import.Tag, Entity SubmissionTag) -> TagView
convertTagInfoToView tagInfo =
  TagView {
     tagViewName = tagName $ entityVal $ fst tagInfo,
     tagViewDescription = tagDescription $ entityVal $ fst tagInfo,
     tagViewAccepted = submissionTagAccepted $ entityVal $ snd tagInfo
     }

convertEvaluationToView :: Map TestReference Evaluation -> Entity Test -> Maybe EvaluationView
convertEvaluationToView theMapping entTest =
  case join $ evaluationScore <$> mEvaluation of
    Just s ->
      Just $ EvaluationView {
        evaluationViewScore = formatTruncatedScore formattingOps mEvaluation,
        evaluationViewFullScore = s,
        evaluationViewTest = testRef
        }
    Nothing -> Nothing
  where mEvaluation = Map.lookup testRef theMapping
        formattingOps = getTestFormattingOpts $ entityVal entTest
        testRef = getTestReference entTest

-- convertTableEntryToView :: Maybe UserId -> [Entity Test] -> TableEntry -> SubmissionView
convertTableEntryToView :: [Entity Test] -> TableEntry -> HandlerFor App SubmissionView
convertTableEntryToView tests entry = do
  mUserId <- maybeAuthPossiblyByToken

  isReevaluable <- runDB $ canBeReevaluated $ entityKey $ tableEntrySubmission entry
  isVisible <- runDB $ checkWhetherVisible submission (entityKey <$> mUserId)

  return $ SubmissionView {
    submissionViewId = fromSqlKey $ entityKey $ tableEntrySubmission entry,
    submissionViewVariantId = fromSqlKey $ entityKey $ tableEntryVariant entry,
    submissionViewRank = tableEntryRank entry,
    submissionViewSubmitter = formatSubmitter $ entityVal $ tableEntrySubmitter entry,
    submissionViewWhen = submissionStamp submission,
    submissionViewVersion = tableEntryVersion entry,
    submissionViewDescription = submissionDescription submission,
    submissionViewTags = Import.map convertTagInfoToView $ tableEntryTagsInfo entry,
    submissionViewHash = fromSHA1ToText $ submissionCommit submission,
    submissionViewEvaluations = catMaybes $ Import.map (convertEvaluationToView $ tableEntryMapping entry) tests,
    submissionViewIsOwner = (entityKey <$> mUserId) == Just (submissionSubmitter submission),
    submissionViewIsReevaluable = isReevaluable,
    submissionViewIsVisible = isVisible,
    submissionViewIsPublic = submissionIsPublic submission
  }
  where submission = entityVal $ tableEntrySubmission entry

fetchChallengeSubmissionsView :: ((Entity Submission) -> Bool) -> Text -> Handler SubmissionsView
fetchChallengeSubmissionsView condition challengeName = do
  Entity challengeId _ <- runDB $ getBy404 $ UniqueName challengeName
  (evaluationMaps, tests') <- runDB $ getChallengeSubmissionInfos 1 condition (const True) id challengeId
  let tests = sortBy testComparator tests'

  submissions <- mapM (convertTableEntryToView tests) evaluationMaps

  return $ SubmissionsView {
    submissionsViewSubmissions = submissions,
    submissionsViewTests = map getTestReference tests
  }


-- TODO switch to fetchChallengeSubmissionSview
getChallengeMySubmissionsR :: Text -> Handler Html
getChallengeMySubmissionsR challengeName = do
  userId <- requireAuthId
  getChallengeSubmissions (\(Entity _ submission) -> (submissionSubmitter submission == userId)) challengeName

getChallengeAllSubmissionsR :: Text -> Handler Html
getChallengeAllSubmissionsR challengeName = getChallengeSubmissions (\_ -> True) challengeName

data EvaluationView = EvaluationView {
  evaluationViewScore :: Text,
  evaluationViewFullScore :: Double,
  evaluationViewTest :: TestReference
}

instance ToJSON EvaluationView where
    toJSON e = object
        [ "score" .= evaluationViewScore e
        , "full-score" .= evaluationViewFullScore e
        , "test" .= evaluationViewTest e
        ]

instance ToSchema EvaluationView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    doubleSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Double)
    testRefSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy TestReference)
    return $ NamedSchema (Just "Evaluation") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("score", stringSchema)
                     , ("full-score", doubleSchema)
                     , ("test", testRefSchema)
                    ]
        & required .~ [ "score", "full-score", "test" ]


data TagView = TagView {
  tagViewName :: Text,
  tagViewDescription :: Maybe Text,
  tagViewAccepted :: Maybe Bool }

instance ToJSON TagView where
  toJSON t = object
       [ "name" .= tagViewName t
       , "description" .= tagViewDescription t
       , "accepted" .= tagViewAccepted t
       ]

instance ToSchema TagView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    return $ NamedSchema (Just "Tag") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("name", stringSchema)
                     , ("description", stringSchema)
                     , ("accepted", boolSchema)
                    ]
        & required .~ [ "name", "description" ]


data SubmissionView = SubmissionView {
  submissionViewId :: Int64,
  submissionViewVariantId :: Int64,
  submissionViewRank :: Int,
  submissionViewSubmitter :: Text,
  submissionViewWhen :: UTCTime,
  submissionViewVersion :: (Int, Int, Int),
  submissionViewDescription :: Text,
  submissionViewTags :: [TagView],
  submissionViewHash :: Text,
  submissionViewEvaluations :: [EvaluationView],
  submissionViewIsOwner :: Bool,
  submissionViewIsReevaluable :: Bool,
  submissionViewIsVisible :: Bool,
  submissionViewIsPublic :: Bool
}

instance ToJSON SubmissionView where
  toJSON s = object
   ["id" .= submissionViewId s
    , "variant" .= submissionViewVariantId s
    , "rank" .= submissionViewRank s
    , "submitter" .= submissionViewSubmitter s
    , "when" .= submissionViewWhen s
    , "version" .= submissionViewVersion s
    , "description" .= submissionViewDescription s
    , "tags" .= submissionViewTags s
    , "hash" .= submissionViewHash s
    , "evaluations" .= submissionViewEvaluations s
    , "isOwner" .= submissionViewIsOwner s
    , "isReevaluable" .= submissionViewIsReevaluable s
    , "isVisible" .= submissionViewIsVisible s
    , "isPublic" .= submissionViewIsPublic s
    ]

instance ToSchema SubmissionView where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy String)
    boolSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Bool)
    intSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy Int)
    intsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [Int])
    tagsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TagView])
    evalsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [EvaluationView])
    return $ NamedSchema (Just "SubmissionView") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("id", intSchema)
                     , ("variant", intSchema)
                     , ("rank", intSchema)
                     , ("submitter", stringSchema)
                     , ("when", stringSchema)
                     , ("version", intsSchema)
                     , ("description", stringSchema)
                     , ("tags", tagsSchema)
                     , ("hash", stringSchema)
                     , ("evaluations", evalsSchema)
                     , ("isOwner", boolSchema)
                     , ("isReevaluable", boolSchema)
                     , ("isVisible", boolSchema)
                     , ("isPublic", boolSchema)
                    ]
        & required .~ [ "id", "variant", "rank", "submitter", "when", "version",
                        "description", "tags", "hash", "evaluations",
                        "isOwner", "isReevaluable", "isVisible", "isPublic" ]

data SubmissionsView = SubmissionsView {
  submissionsViewSubmissions :: [SubmissionView],
  submissionsViewTests :: [TestReference]
}

instance ToJSON SubmissionsView where
  toJSON ss = object
    [ "tests" .= submissionsViewTests ss,
      "submissions" .= submissionsViewSubmissions ss
    ]

instance ToSchema SubmissionsView where
  declareNamedSchema _ = do
    submissionViewsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [SubmissionView])
    testRefsSchema <- declareSchemaRef (DPR.Proxy :: DPR.Proxy [TestReference])
    return $ NamedSchema (Just "Tag") $ mempty
        & type_ .~ SwaggerObject
        & properties .~
           fromList [  ("submissions", submissionViewsSchema)
                     , ("tests", testRefsSchema)
                    ]
        & required .~ [ "tests", "submission" ]

getChallengeSubmissions :: ((Entity Submission) -> Bool) -> Text -> Handler Html
getChallengeSubmissions condition challengeName = do
  Entity challengeId challenge <- runDB $ getBy404 $ UniqueName challengeName
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
  theVersion <- runDB $ getBy404 $ UniqueVersionByCommit $ challengeVersion challenge
  let versionFormatted = formatVersion ((versionMajor $ entityVal theVersion),
                                        (versionMinor $ entityVal theVersion),
                                        (versionPatch $ entityVal theVersion))
  maybeUser <- maybeAuth
  bc <- widgetToPageContent widget
  defaultLayout $ do
    setTitle "Challenge"
    $(widgetFile "challenge")
