module Handler.Graph where

import Import

import Handler.Tables
import Handler.Shared (formatParameter, formatScore, getMainTest)
import Data.Maybe
import Data.List ((!!))
import Database.Persist.Sql
import GEval.Core (MetricValue)
import qualified Data.Map as M


getChallengeGraphDataR :: Text -> Handler Value
getChallengeGraphDataR challengeName = submissionsToJSON (\_ -> True) challengeName

data ParamGraphItem = ParamGraphItem TableEntry Text Text MetricValue

data ParamGraphSeries = ParamGraphSeries Text [(TableEntry, Text, MetricValue)]

getChallengeParamGraphDataR :: Text -> (Key Test) -> Text -> Handler Value
getChallengeParamGraphDataR challengeName testId paramName = do
  (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName
  test <- runDB $ get404 testId

  (entries, _) <- getChallengeSubmissionInfos (const True) challengeId

  let values = map (findParamValue paramName) entries

  let items = Data.Maybe.catMaybes $ map (toParamGraphItem testId paramName) $ zip entries values

  let series = map (\(label, rs) -> ParamGraphSeries label rs)
               $ organizeBy
               $ map (\(ParamGraphItem entry label x y) -> (label, (entry, x, y))) items

  return $ object [
    "xs" .= object (map (\(ParamGraphSeries seriesName _) -> (seriesName .= (xSeriesName seriesName))) series),
    "columns" .= ((map (toYColumn $ testPrecision test) series) ++ (map toXColumn series))
                  ]
toYColumn :: Maybe Int -> ParamGraphSeries -> [Text]
toYColumn mPrecision (ParamGraphSeries seriesName items) =
  seriesName : (map (\(_,_,v) -> formatScore mPrecision v) items)

toXColumn :: ParamGraphSeries -> [Text]
toXColumn (ParamGraphSeries seriesName items) =
  (xSeriesName seriesName) : (map (\(_,x,_) -> x) items)

xSeriesName :: Text -> Text
xSeriesName = (++ "_x")

organizeBy :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
organizeBy pList = M.toList $ M.fromListWith (++) $ map (\(x, y) -> (x, [y])) pList

toParamGraphItem :: TestId -> Text -> (TableEntry, Maybe Text) -> Maybe ParamGraphItem
toParamGraphItem _ _ (_, Nothing) = Nothing
toParamGraphItem tid paramName (entry, Just val) = (ParamGraphItem entry label val) <$> join y
  where label = unwords (tagsFormatted ++ paramsFormatted)
        tagsFormatted =
                map (tagName . entityVal . fst)
                $ tableEntryTagsInfo entry
        paramsFormatted =
                map formatParameter
                $ filter (\pe -> parameterName pe /= paramName)
                $ map entityVal $ tableEntryParams entry
        y = evaluationScore <$> lookup tid (tableEntryMapping entry)

findParamValue :: Text -> TableEntry -> Maybe Text
findParamValue paramName entry =
  (parameterValue . entityVal) <$> (find (\e -> parameterName (entityVal e) == paramName) $ tableEntryParams entry)

submissionsToJSON :: ((Entity Submission) -> Bool) -> Text -> Handler Value
submissionsToJSON condition challengeName = do
  (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName

  (entries, _) <- getLeaderboardEntriesByCriterion challengeId
                                                  condition
                                                  (\(TableEntry (Entity submissionId _) _ _ _ _ _) -> [submissionId])


  tests <- runDB $ selectList [TestChallenge ==. challengeId] []
  let mainTestId = entityKey $ getMainTest tests

  let naturalRange = getNaturalRange mainTestId entries
  let submissionIds = map leaderboardBestSubmissionId entries

  forks <- runDB $ selectList [ForkSource <-. submissionIds, ForkTarget <-. submissionIds] []

  return $ object [ "nodes" .= (Data.Maybe.catMaybes
                                $ map (auxSubmissionToNode mainTestId naturalRange)
                                $ entries),
                    "edges" .= map forkToEdge forks ]

getNaturalRange :: TestId -> [LeaderboardEntry] -> Double
getNaturalRange testId entries = 2.0 * (interQuantile
                                        $ Data.Maybe.catMaybes
                                        $ map (\entry -> evaluationScore $ ((leaderboardEvaluationMap entry) M.! testId)) entries)

auxSubmissionToNode :: TestId -> Double -> LeaderboardEntry -> Maybe Value
auxSubmissionToNode testId naturalRange entry = case evaluationScore $ ((leaderboardEvaluationMap entry) M.! testId) of
  Just score ->  Just $ object [
    "id" .= (nodeId $ leaderboardBestSubmissionId entry),
    "x"  .= (stampToX $ submissionStamp $ leaderboardBestSubmission entry),
    "y"  .= (- ((score / naturalRange) * 100.0)),
    "size" .= (2 :: Int),
    "label" .= descriptionToBeShown (leaderboardBestSubmission entry) (leaderboardBestVariant entry) (leaderboardParams entry) ]
  Nothing -> Nothing

forkToEdge :: Entity Fork -> Value
forkToEdge (Entity forkId fork) = object [
  "source" .= nodeId (forkSource fork),
  "target" .= nodeId (forkTarget fork),
  "id"     .= edgeId forkId,
  "type"   .= ["arrow" :: String]
  ]

nodeId :: Key Submission -> String
nodeId = ("n" ++) . show . fromSqlKey

edgeId :: Key Fork -> String
edgeId = ("e" ++) . show . fromSqlKey

stampToX :: UTCTime -> Integer
stampToX = toModifiedJulianDay . utctDay

-- taken from Math.Statistics

interQuantile :: (Fractional b, Ord b) => [b] -> b
interQuantile [] = 10.0
interQuantile xs = (q' - q)
    where q = quantile 0.25 xs
          q' = quantile 0.75 xs

quantile :: (Fractional b, Ord b) => Double -> [b] -> b
quantile q = quantileAsc q . sort

quantileAsc :: (Fractional b, Ord b) => Double -> [b] -> b
quantileAsc _ [] = error "x"
quantileAsc q xs
    | q < 0 || q > 1 = error "quantile out of range"
    | otherwise = xs !! (quantIndex (length xs) q)
    where quantIndex :: Int -> Double -> Int
          quantIndex len q' = case round $ q' * (fromIntegral len - 1) of
                               idx | idx < 0    -> error "Quantile index too small"
                                   | idx >= len -> error "Quantile index too large"
                                   | otherwise  -> idx
