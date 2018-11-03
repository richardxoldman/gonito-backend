{-# LANGUAGE PackageImports #-}

module Gonito.ExtractMetadata (
  extractMetadataFromRepoDir,
  GonitoMetadata(..),
  ExtractionOptions(..),
  parseCommitMessage,
  getLastCommitMessage,
  parseTags)
   where

import Import

import Data.Attoparsec.Text
import Data.Text
import qualified Data.Text.Encoding as DTE

import Data.Aeson
import qualified Data.Yaml as Y

import System.Exit
import System.Process

import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

import Handler.Shared (gitPath)

import "Glob" System.FilePath.Glob as G

data ExtractionOptions = ExtractionOptions {
  extractionOptionsDescription :: Maybe Text,
  extractionOptionsTags :: Maybe Text,
  extractionOptionsGeneralParams :: Maybe (H.HashMap Text Text),
  extractionOptionsUnwantedParams :: Maybe [Text],
  extractionOptionsParamFiles :: Maybe [String],
  extractionOptionsMLRunPath :: Maybe FilePath
  }

instance FromJSON ExtractionOptions where
    parseJSON = withObject "ExtractionOptions" $ \v -> ExtractionOptions
        <$> v .:? "description"
        <*> v .:? "tags"
        <*> fmap (fmap enforceTextHash) (v .:? "params")
        <*> v .:? "unwanted-params"
        <*> v .:? "param-files"
        <*> v .:? "mlrun-path"

instance Default ExtractionOptions where
  def = ExtractionOptions {
    extractionOptionsDescription = Nothing,
    extractionOptionsTags = Nothing,
    extractionOptionsGeneralParams = Nothing,
    extractionOptionsUnwantedParams = Nothing,
    extractionOptionsParamFiles = Nothing,
    extractionOptionsMLRunPath = Nothing
    }

data GonitoMetadata = GonitoMetadata {
  gonitoMetadataDescription :: Text,
  gonitoMetadataTags :: S.Set Text,
  gonitoMetadataGeneralParams :: H.HashMap Text Text
  }
  deriving (Eq, Show)

gonitoYamlFile :: FilePath
gonitoYamlFile = "gonito.yaml"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

combineExtractionOptions :: Maybe ExtractionOptions -> ExtractionOptions -> ExtractionOptions
combineExtractionOptions Nothing options = options
combineExtractionOptions (Just otherOptions) options = ExtractionOptions {
  extractionOptionsDescription = combineWithT extractionOptionsDescription,
  extractionOptionsTags = combineWithT extractionOptionsTags,
  extractionOptionsGeneralParams = Just $ (fromMaybe H.empty $ extractionOptionsGeneralParams options)
                                          `H.union`
                                          (fromMaybe H.empty $ extractionOptionsGeneralParams otherOptions),
  extractionOptionsUnwantedParams = Just $ (fromMaybe [] $ extractionOptionsUnwantedParams options)
                                           ++
                                           (fromMaybe [] $ extractionOptionsUnwantedParams otherOptions),
  extractionOptionsParamFiles = case extractionOptionsParamFiles options of
                                  Nothing -> extractionOptionsParamFiles otherOptions
                                  Just pfs -> Just pfs,
  extractionOptionsMLRunPath = combineWithF extractionOptionsMLRunPath }
  where combineWithT fun = case fun options of
                            Nothing -> fun otherOptions
                            Just v -> Just v
        combineWithF fun = case fun options of
                            Nothing -> fun otherOptions
                            Just v -> Just v

extractMetadataFromRepoDir :: FilePath -> ExtractionOptions -> IO GonitoMetadata
extractMetadataFromRepoDir repoDir formExtractionOptions = do
  commitMessage <- getLastCommitMessage repoDir
  let (mCommitDescription, mCommitTags) = parseCommitMessage commitMessage

  mGonitoYamlOptions <- eitherToMaybe <$> Y.decodeFileEither (repoDir </> gonitoYamlFile)

  let extractionOptions = combineExtractionOptions mGonitoYamlOptions formExtractionOptions

  let description = case extractionOptionsDescription extractionOptions of
                      Just d -> d
                      Nothing -> case mCommitDescription of
                        Just d -> d
                        Nothing -> "???"

  let commitTagsParsed = parseTags mCommitTags
  let formTagsParsed = parseTags $ extractionOptionsTags extractionOptions
  let tagsParsed = union commitTagsParsed formTagsParsed

  paramFiles <- case extractionOptionsParamFiles extractionOptions of
    Just paramFilesGlobs -> G.globDir (Import.map G.compile $ traceShowId paramFilesGlobs) repoDir
    Nothing -> pure []

  params' <- H.unions <$> (mapM parseParamFile
                         $ traceShowId
                         $ Import.filter (/= (repoDir </> gonitoYamlFile))
                         $ Import.concat paramFiles)
  let params =
        Import.foldl' (flip H.delete) params' (fromMaybe [] $ extractionOptionsUnwantedParams extractionOptions)
        `H.union`
        fromMaybe H.empty (extractionOptionsGeneralParams extractionOptions)

  pure $ GonitoMetadata {
    gonitoMetadataDescription = description,
    gonitoMetadataTags = tagsParsed,
    gonitoMetadataGeneralParams = params
  }


parseParamFile :: FilePath -> IO (H.HashMap Text Text)
parseParamFile yamlFile = do
  decoded <- Y.decodeFileEither yamlFile

  return $ case decoded of
    Left _ -> H.empty
    Right h -> enforceTextHash h

enforceTextHash :: H.HashMap Text Value -> H.HashMap Text Text
enforceTextHash h = H.fromList
                    $ Import.map (\(p, pv) -> (p, strip $ DTE.decodeUtf8 $ Y.encode pv))
                    $ H.toList h

getLastCommitMessage :: FilePath -> IO (Maybe Text)
getLastCommitMessage repoDir = do
  (exitCode, out) <- runProgram repoDir gitPath ["log", "-1", "--pretty=%B"]
  return $ case exitCode of
             ExitSuccess -> Just out
             ExitFailure _ -> Nothing

runProgram :: FilePath -> FilePath -> [String] -> IO (ExitCode, Text)
runProgram dir prog args = do
  (_, o, _, p) <- runInteractiveProcess prog args (Just dir) Nothing
  hSetBuffering o NoBuffering
  out <- hGetContents o
  exitCode <- Import.length out `seq` waitForProcess p
  return (exitCode, decodeUtf8 out)

parseTags :: Maybe Text -> S.Set Text
parseTags (Just tags) = S.fromList $ Import.map Data.Text.strip $ Data.Text.split (== ',') tags
parseTags Nothing = S.empty

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
  _ <- (string "tags" <|> string "labels" <|> string "Tags" <|> string "Labels")
  _ <- char ':'
  skipMany space
  s <- many notEndOfLine
  endOfLine
  return $ Data.Text.pack s

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

emptyLine :: Data.Attoparsec.Text.Parser ()
emptyLine = do
  many space *> endOfLine
