{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SubmissionPreview where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.), (<.>))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import qualified Data.Text                  as T
import qualified Data.Conduit.List          as CL
import           GEval.Common (FormattingOptions(..), MetricValue)
import           GEval.Core (GEvalSpecification(..), GEvalOptions(..), ResultOrdering(..))
import           GEval.LineByLine (runLineByLineGeneralized, LineRecord(..))
import           Data.Diff (Diff(..), runDiff, current)
import           System.FilePath (takeFileName)
import           Data.Maybe (fromJust)
import           GEval.OptionsParser (readOptsFromConfigFile)
import qualified System.Directory           as D
import           Data.Conduit.SmartSource (lookForCompressedFiles)
import           Handler.Tables
import           Handler.Shared
import           Handler.SubmissionView
import           PersistSHA1
import qualified Database.Esqueleto         as E
import           Database.Esqueleto      ((^.))
import qualified Data.Map.Lazy              as LM
import           Data.List (nub)
import Control.Lens.Internal.Level (Level(One))
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)


import qualified Yesod.Table as Table




data SourceOfExpected = NoExpectedFound | ExpectedFromSubmission | ExpectedFromChallenge
    deriving (Eq, Show)


data DiffLineRecord = DiffLineRecord Text Text (Diff (Text, MetricValue)) Word32
    deriving (Show)


data ViewVariantData = ViewVariantData
    { viewVariantDataFullSubmissionInfo :: (FullSubmissionInfo, Maybe Text)
    , viewVariantDataTableEntry :: TableEntry
    , viewVariantDataTests :: [Entity Test]
    , viewVariantDataOuts :: [(SHA1, Text)]
    }


getUniLineRecord :: LineRecord -> DiffLineRecord
getUniLineRecord (LineRecord inp expect out lineNo val) = DiffLineRecord inp expect (OneThing (out, val)) lineNo


checkForExpected :: FilePath -> FilePath -> IO Bool
checkForExpected repoDir testName = do
    expFile <- lookForCompressedFiles (repoDir </> testName </> "expected.tsv")
    D.doesFileExist expFile


getOut :: Maybe UserId -> TableEntry -> Handler (Maybe (FilePath, FilePath))
getOut _ entry = do
    let variant = variantName $ entityVal $ tableEntryVariant entry
        isViewable = True

    if isViewable
        then do
            mRepoDir <- justGetSubmissionRepoDir $ entityKey $ tableEntrySubmission entry

            case mRepoDir of
                Just repoDir -> do
                    outFilePath <- liftIO $ lookForCompressedFiles (repoDir </> T.unpack variant <.> "tsv")
                    pure $ Just (repoDir, outFilePath)
                Nothing -> pure Nothing

        else do
          pure Nothing


-- Return the submission version for which the tests are available.
-- It should be simply submissionVersion but the problem is that
-- we change the update the change version when updating a challenge
-- (if the test did not change), and we are left sometimes with dangling
-- versions for which no tests are available.
realSubmissionVersion :: Entity Submission -> Handler SHA1
realSubmissionVersion (Entity submissionId _) = do
  testOutputs <- runDB $ E.select
                        $ E.from $ \(variant, out, test, evaluation, ver) -> do
                          E.where_ (variant ^. VariantSubmission E.==. E.val submissionId
                                    E.&&. out ^. OutVariant E.==. variant ^. VariantId
                                    E.&&. out ^. OutTest E.==. test ^. TestId
                                    E.&&. out ^. OutChecksum E.==. evaluation ^. EvaluationChecksum
                                    E.&&. evaluation ^. EvaluationVersion E.==. ver ^. VersionCommit)
                          E.orderBy [E.desc (ver ^. VersionStamp)]
                          return test

  let (t:_) = testOutputs
  return $ testCommit $ entityVal t


fetchViewVariantData :: VariantId -> Handler ViewVariantData
fetchViewVariantData variantId = do
    variant <- runDB $ get404 variantId
    let theSubmissionId = variantSubmission variant
    theSubmission <- runDB $ get404 theSubmissionId

    theVersion <- realSubmissionVersion $ Entity theSubmissionId theSubmission

    let priorityLimitForViewVariant = 4

    entryTests <- runDB $ getChallengeSubmissionInfosForVersion
        priorityLimitForViewVariant
        (\e -> entityKey e == theSubmissionId)
        (\e -> entityKey e == variantId)
        id
        (submissionChallenge theSubmission)
        theVersion

    let ([entry], tests') = entryTests
        tests = sortBy (flip testComparator) tests'


    fullSubmissionInfo <- getFullInfo (Entity theSubmissionId theSubmission)

    testOutputs <- runDB $ E.select $ E.from $ \(out, test) -> do
            E.where_ (
                out ^. OutTest E.==. test ^. TestId E.&&.
                out ^. OutVariant E.==. E.val variantId
                )
            E.orderBy []
            return (out, test)

    let outputs =
          sortBy (\a b -> snd b `compare` snd a)
          $ nub
          $ map (\(out, test) -> (outChecksum $ entityVal out, testName $ entityVal test)) testOutputs

    return $ ViewVariantData (fullSubmissionInfo, Just $ variantName variant) entry tests outputs


nullSHA1 :: SHA1
nullSHA1 = fromTextToSHA1 "da39a3ee5e6b4b0d3255bfef95601890afd80709"


-- viewOutputWithNonDefaultTestSelected entry tests mainTest (outputHash, testSet) = do
--getSubmissionPreviewR :: VariantId -> SubmissionId -> Handler Text
getSubmissionPreviewR variantId submissionId = do
    -- TO GET -------------------
    let testId = undefined
    -----------------------------

    let variantIdDiff = OneThing variantId


    variantInfos <- mapM fetchViewVariantData variantIdDiff
    let fullSubmissionInfo = viewVariantDataFullSubmissionInfo <$> variantInfos
        entry = viewVariantDataTableEntry <$> variantInfos
        tests' = viewVariantDataTests <$> variantInfos
        outputs' = viewVariantDataOuts <$> variantInfos

    let testIds = map fst $ runDiff () $ fmap (map entityKey) tests'
    testEnts <- mapM (runDB . get404) testIds
    let tests = zipWith Entity testIds testEnts

        outputs :: [(Diff SHA1, Text)] =
            sortBy (\a b -> snd b `compare` snd a)
            $ map swap $ LM.toList
            $ runDiff (nullSHA1, ())
            $ fmap (LM.fromList . map swap) outputs'

        outQueryForm = renderBootstrap3 BootstrapBasicForm $ areq textField (fieldSettingsLabel MsgOutSha1) Nothing

    (formWidget, formEnctype) <- generateFormPost outQueryForm


    -- To get from the variant viewer
    let testSet = undefined

    testSelected <- runDB $ get404 testId
    let mainTest = Entity testId testSelected
    ---------------------------------

    mauthId <- maybeAuthId
    outPaths <- mapM (getOut mauthId) entry

    let repoDir = fst . fromJust <$> outPaths
        testName = T.unpack testSet
        challengeId = submissionChallenge $ entityVal $ tableEntrySubmission $ current entry
    
    challenge <- runDB $ get404 challengeId
    challengeRepoDir <- getRepoDir (challengePublicRepo challenge)
    expFileExistsInChallengeRepo <- liftIO $ checkForExpected challengeRepoDir testName
    expFileExists <- liftIO $ checkForExpected (current repoDir) testName

    let expFileStatus
            | expFileExists = ExpectedFromSubmission
            | expFileExistsInChallengeRepo = ExpectedFromChallenge
            | otherwise = NoExpectedFound

    let theRepoDir = case expFileStatus of
            ExpectedFromSubmission -> current repoDir
            ExpectedFromChallenge -> challengeRepoDir
            NoExpectedFound -> error "Should not be here"

        mainMetric = testMetric $ entityVal mainTest
        outFilePath = snd . fromJust <$> outPaths
        outFile = takeFileName $ current outFilePath

    rightOpts <- liftIO $ readOptsFromConfigFile [] (theRepoDir </> "config.txt")

    let Right opts = rightOpts

        maximumNumberOfItemsToBeShown = 40

        spec = GEvalSpecification
            { gesOutDirectory = current repoDir
            , gesExpectedDirectory = Just theRepoDir
            , gesTestName = testName
            , gesSelector = Nothing
            , gesOutFile = outFile
            , gesAltOutFiles = Nothing
            , gesExpectedFile = "expected.tsv"
            , gesInputFile = "in.tsv"
            , gesMetrics = [mainMetric]
            , gesFormatting = FormattingOptions
                { decimalPlaces = Nothing
                , asPercentage = False
                }
            , gesTokenizer = Nothing
            , gesGonitoHost = Nothing
            , gesToken = Nothing
            , gesGonitoGitAnnexRemote = Nothing
            , gesReferences = Nothing
            , gesBootstrapResampling = Nothing
            , gesInHeader = gesInHeader $ geoSpec opts
            , gesOutHeader = gesOutHeader $ geoSpec opts
            , gesShowPreprocessed = True
            }

    result <- liftIO $ runLineByLineGeneralized FirstTheWorst spec (\_ -> CL.take maximumNumberOfItemsToBeShown)
    pure $ T.pack $ show $ zip [1..] $ map getUniLineRecord result


submissionPreviewApi :: Swagger
submissionPreviewApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareSubmissionPreviewApi mempty


declareSubmissionPreviewApi :: Declare (Definitions Schema) Swagger
declareSubmissionPreviewApi = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/submission-preview/{submissionId}", mempty
            & DS.get ?~ (mempty
                & parameters .~ 
                [
                    Inline $ mempty
                    & name .~ "variantId"
                    & required ?~ True
                    & description ?~ "Integer, e.g.: 123"
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ idSchema
                        ),
                    Inline $ mempty
                    & name .~ "submissionId"
                    & required ?~ True
                    & description ?~ "Integer, e.g.: 123"
                    & schema .~ ParamOther (mempty
                        & in_ .~ ParamPath
                        & paramSchema .~ idSchema
                        )
                ]
                & produces ?~ MimeList ["application/json"]
                & description ?~ "Get information about a submission."
                & at 200 ?~ Inline response
                )
            )
        ]
