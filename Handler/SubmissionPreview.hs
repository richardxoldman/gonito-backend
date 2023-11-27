module Handler.SubmissionPreview where

import           Import                     hiding (Proxy, encodeUtf8, fromList)

import           Control.Lens               hiding ((.=), (^.), (<.>))
import           Data.HashMap.Strict.InsOrd (fromList)
import           Data.Proxy                 as DPR
import           Data.Swagger               hiding (delete, get, tags)
import qualified Data.Swagger               as DS
import           Data.Swagger.Declare

import           Handler.MakePublic

import           Data.List (intersect)
import qualified Data.Text as T
import           Database.Persist.Sql       hiding (insert)
import           Prelude                    (read)
import qualified Data.Conduit.List as CL
import           GEval.Common (FormattingOptions(..), MetricValue)
import           GEval.Core (GEvalSpecification(..), GEvalOptions(..), ResultOrdering(..))
import           GEval.LineByLine (runLineByLineGeneralized, runDiffGeneralized, LineRecord(..))
import           Data.Diff
import           System.FilePath (takeFileName)
import           Data.Maybe (fromJust)
import           GEval.OptionsParser (readOptsFromConfigFile)
import qualified System.Directory as D
import           Data.Conduit.SmartSource (lookForCompressedFiles)
import           Handler.Tables
import           Handler.Shared


data SourceOfExpected = NoExpectedFound | ExpectedFromSubmission | ExpectedFromChallenge
  deriving (Eq, Show)


data DiffLineRecord = DiffLineRecord Text Text (Diff (Text, MetricValue)) Word32
                      deriving (Show)


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


getSubmissionPreviewR :: VariantId -> SubmissionId -> Handler Html
getSubmissionPreviewR variantId submissionId = do

    -- To get from the variant viewer
    let testSet = undefined
    let mainTest = undefined
    let entry = undefined
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
    pure $ zip [1..] $ map getUniLineRecord result


submissionPreviewApi :: Swagger
submissionPreviewApi = spec & definitions .~ defs
    where
        (defs, spec) = runDeclare declareSubmissionPreviewApi mempty


declareSubmissionPreviewApi :: Declare (Definitions Schema) Swagger
declareSubmissionPreviewApi = do
    let idSchema = toParamSchema (Proxy :: Proxy Int)
        txtSchema = toParamSchema (Proxy :: Proxy Text)
    response <- declareResponse (Proxy :: Proxy ())

    pure $ mempty & paths .~ fromList
        [
            ("/api/submission-preview/{submissionId}", mempty
            & DS.get ?~ (mempty
                & parameters .~ 
                [
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
