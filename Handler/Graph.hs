module Handler.Graph where

import Import

import Handler.Tables
import Handler.Shared (getMainTest, formatParameter)
import Data.Maybe
import Data.List ((!!))
import Database.Persist.Sql
import GEval.Core (MetricValue)
import qualified Data.Map as M


getChallengeGraphDataR :: Text -> Handler Value
getChallengeGraphDataR challengeName = submissionsToJSON (\_ -> True) challengeName

data ParamGraphItem = ParamGraphItem TableEntry Text Text MetricValue

data ParamGraphSeries = ParamGraphSeries Text [(TableEntry, Text, MetricValue)]

getChallengeParamGraphDataR :: Text -> Text -> Handler Value
getChallengeParamGraphDataR challengeName paramName = do
  (Entity challengeId _) <- runDB $ getBy404 $ UniqueName challengeName

  (entries, tests) <- getChallengeSubmissionInfos (const True) challengeId

  let mainTestId = entityKey $ getMainTest tests

  let values = map (findParamValue paramName) entries

  let items = Data.Maybe.catMaybes $ map (toParamGraphItem mainTestId paramName) $ zip entries values

  let series = map (\(label, rs) -> ParamGraphSeries label rs)
               $ organizeBy
               $ map (\(ParamGraphItem entry label x y) -> (label, (entry, x, y))) items

  return $ object [
    "xs" .= object (map (\(ParamGraphSeries seriesName _) -> (seriesName .= (xSeriesName seriesName))) series),
    "columns" .= ((map toYColumn series) ++ (map toXColumn series))
                  ]
toYColumn :: ParamGraphSeries -> [Text]
toYColumn (ParamGraphSeries seriesName items) =
  seriesName : (map (\(_,_,v) -> pack $ show v) items)

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

  (_, entries) <- getLeaderboardEntriesByCriterion challengeId condition (\(TableEntry (Entity submissionId _) _ _ _ _ _) -> submissionId)

  let naturalRange = getNaturalRange entries
  let submissionIds = map leaderboardBestSubmissionId entries

  forks <- runDB $ selectList [ForkSource <-. submissionIds, ForkTarget <-. submissionIds] []

  return $ object [ "nodes" .= (Data.Maybe.catMaybes $ map (auxSubmissionToNode naturalRange) $ entries),
                    "edges" .= map forkToEdge forks ]

getNaturalRange :: [LeaderboardEntry] -> Double
getNaturalRange entries = 2.0 * (interQuantile $ Data.Maybe.catMaybes $ map (evaluationScore . leaderboardEvaluation) entries)

auxSubmissionToNode :: Double -> LeaderboardEntry -> Maybe Value
auxSubmissionToNode naturalRange entry = case evaluationScore $ leaderboardEvaluation entry of
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
          quantIndex len q = case round $ q * (fromIntegral len - 1) of
                               idx | idx < 0    -> error "Quantile index too small"
                                   | idx >= len -> error "Quantile index too large"
                                   | otherwise  -> idx
