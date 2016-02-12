module Handler.Graph where

import Import

import Handler.Tables
import Data.Maybe
import Data.List ((!!))
import Database.Persist.Sql

getChallengeGraphDataR :: Text -> Handler Value
getChallengeGraphDataR challengeName = submissionsToJSON (\_ -> True) challengeName

submissionsToJSON :: ((Entity Submission) -> Bool) -> Text -> Handler Value
submissionsToJSON condition challengeName = do
  challengeEnt@(Entity challengeId challenge) <- runDB $ getBy404 $ UniqueName challengeName
  (evaluationMaps, tests) <- getChallengeSubmissionInfos condition challengeId
  let mainTestEnt = getMainTest tests
  let (Entity mainTestId mainTest) = mainTestEnt
  let auxSubmissions = getAuxSubmissionEnts mainTestId evaluationMaps
  let naturalRange = getNaturalRange auxSubmissions
  let submissionIds = map (\(Entity k _, _) -> k) $ concat $ map (\(_, (_, p)) -> p) auxSubmissions

  forks <- runDB $ selectList [ForkSource <-. submissionIds, ForkTarget <-. submissionIds] []

  return $ object [ "nodes" .= (Data.Maybe.catMaybes $ map (auxSubmissionToNode naturalRange) $ zip [0..] auxSubmissions),
                    "edges" .= map forkToEdge forks ]

getNaturalRange auxSubmissions = (2.0 * (interQuantile $ Data.Maybe.catMaybes $ map getScore auxSubmissions))

getScore (_, (_, [])) = Nothing
getScore (_, (_, [(_, evaluation)])) = evaluationScore evaluation

auxSubmissionToNode :: Double -> (Int, (Key User, (User, [(Entity Submission, Evaluation)]))) -> Maybe Value
auxSubmissionToNode _ (_, (_, (_, []))) = Nothing
auxSubmissionToNode naturalRange (n, (_, (_, [(Entity submissionId submission, evaluation)]))) = case evaluationScore evaluation of
  Just score ->  Just $ object [
    "id" .= nodeId submissionId,
    "x"  .= (stampToX $ submissionStamp submission),
    "y"  .= (- ((score / naturalRange) * 100.0)),
    "size" .= (2 :: Int),
    "label" .= submissionDescription submission ]
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
