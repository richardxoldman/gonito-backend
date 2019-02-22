module Handler.Dashboard where

import Import

import Data.Time.LocalTime
import Handler.Shared
import Handler.Common (checkIfAdmin)
import Handler.Tables

import Data.SubmissionConditions (parseCondition, checkCondition, VariantEntry(..))

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3, bfs)
import qualified Yesod.Table as Table

import qualified Data.Text as T
import qualified Data.Map as M

import Handler.Tables (timestampCell)
import GEval.Core (isBetter)

data IndicatorEntry = IndicatorEntry {
  indicatorEntryIndicator :: Entity Indicator,
  indicatorEntryTest :: Entity Test,
  indicatorEntryChallenge :: Entity Challenge,
  indicatorEntryFilterCondition :: Maybe Text,
  indicatorEntryTargetCondition :: Maybe Text,
  indicatorEntryTargets :: [Entity Target]
}

data TargetStatus = TargetPassed | TargetFailed | TargetOngoing
                    deriving (Eq, Show)

getDashboardR :: Handler Html
getDashboardR = do
  (formWidget, formEnctype) <- generateFormPost targetForm
  mUser <- maybeAuth
  doDashboard mUser formWidget formEnctype

postDashboardR :: Handler Html
postDashboardR = do
  ((result, formWidget), formEnctype) <- runFormPost targetForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess (testId, mName, filterCondition, targetCondition, deadlineDay, deadlineTime, value) -> do
        targetId <- runDB $ insert $ Indicator testId filterCondition targetCondition
        _ <- runDB $ insert $ Target targetId (UTCTime { utctDay = deadlineDay, utctDayTime = timeOfDayToTime deadlineTime }) value mName
        return ()
      _ -> do
           return ()
  doDashboard mUser formWidget formEnctype

getDeleteIndicatorR :: IndicatorId -> Handler Html
getDeleteIndicatorR indicatorId = do
  (formWidget, formEnctype) <- generateFormPost targetForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ runDB $ do
    targets <- selectList [TargetIndicator ==. indicatorId] []
    mapM_ delete $ map entityKey targets
    delete indicatorId
  setMessage $ toHtml (("Indicator deleted along with its targets!" :: Text))
  doDashboard mUser formWidget formEnctype

getEditIndicatorR :: IndicatorId -> Handler Html
getEditIndicatorR indicatorId = do
  indicator <- runDB $ get404 indicatorId
  (formWidget, formEnctype) <- generateFormPost (indicatorForm indicator)
  mUser <- maybeAuth
  doEditIndicator mUser indicatorId formWidget formEnctype

postEditIndicatorR :: IndicatorId -> Handler Html
postEditIndicatorR indicatorId = do
  indicator <- runDB $ get404 indicatorId
  ((result, formWidget), formEnctype) <- runFormPost (indicatorForm indicator)
  mUser <- maybeAuth

  when (checkIfAdmin mUser) $ do
     case result of
      FormSuccess changedIndicator -> do
        runDB $ replace indicatorId changedIndicator
        return ()
      _ -> do
           return ()

  doEditIndicator mUser indicatorId formWidget formEnctype

doEditIndicator mUser indicatorId formWidget formEnctype = do
  (addTargetformWidget, addTargetFormEnctype) <- generateFormPost addTargetForm

  indicator <- runDB $ get404 indicatorId
  indicatorEntry <- indicatorToEntry (Entity indicatorId indicator)
  let mPrecision = testPrecision $ entityVal $ indicatorEntryTest indicatorEntry
  defaultLayout $ do
    setTitle "Dashboard"
    $(widgetFile "edit-indicator")

postAddTargetR :: IndicatorId -> Handler Html
postAddTargetR indicatorId = do
  ((result, _), _) <- runFormPost addTargetForm
  mUser <- maybeAuth
  when (checkIfAdmin mUser) $ runDB $ do
     case result of
      FormSuccess (mName, deadlineDay, deadlineTime, value) -> do
        _ <- insert $ Target indicatorId (UTCTime { utctDay = deadlineDay, utctDayTime = timeOfDayToTime deadlineTime }) value mName
        return ()
      _ -> do
           return ()
  getEditIndicatorR indicatorId


getDeleteTargetR :: TargetId -> Handler Html
getDeleteTargetR targetId = do
  (formWidget, formEnctype) <- generateFormPost targetForm
  mUser <- maybeAuth
  target <- runDB $ get404 targetId
  when (checkIfAdmin mUser) $ runDB $ do
    delete targetId
  setMessage $ toHtml (("Target deleted!" :: Text))
  doEditIndicator mUser (targetIndicator target) formWidget formEnctype


doDashboard mUser formWidget formEnctype = do
  indicators <- runDB $ selectList [] [Asc IndicatorId]

  indicatorEntries <- mapM indicatorToEntry indicators
  let indicatorJSs = getIndicatorChartJss indicatorEntries

  defaultLayout $ do
    setTitle "Dashboard"
    $(widgetFile "dashboard")

getIndicatorChartJss :: [IndicatorEntry] -> JavascriptUrl (Route App)
getIndicatorChartJss entries =
  mconcat $ map (getIndicatorChartJs . entityKey . indicatorEntryIndicator) entries

getIndicatorChartJs :: IndicatorId -> JavascriptUrl (Route App)
getIndicatorChartJs indicatorId = [julius|
$.getJSON("@{IndicatorGraphDataR indicatorId}", function(data) {
        c3.generate(data) });
|]

indicatorToEntry :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, PersistQueryRead (YesodPersistBackend site), YesodPersist site) => Entity Indicator -> HandlerFor site IndicatorEntry
indicatorToEntry indicatorEnt@(Entity indicatorId indicator) = runDB $ do
  let theTestId = indicatorTest indicator
  test <- get404 theTestId

  let theChallengeId = testChallenge test
  challenge <- get404 theChallengeId

  targets <- selectList [TargetIndicator ==. indicatorId] [Asc TargetDeadline]

  return $ IndicatorEntry {
    indicatorEntryIndicator = indicatorEnt,
    indicatorEntryTest = Entity theTestId test,
    indicatorEntryChallenge = Entity theChallengeId challenge,
    indicatorEntryFilterCondition = indicatorFilterCondition indicator,
    indicatorEntryTargetCondition = indicatorTargetCondition indicator,
    indicatorEntryTargets = targets
    }

targetForm :: Form (TestId, Maybe Text, Maybe Text, Maybe Text, Day, TimeOfDay, Double)
targetForm = renderBootstrap3 BootstrapBasicForm $ (,,,,,,)
    <$> testSelectFieldList Nothing
    <*> aopt textField (bfs MsgTargetName) Nothing
    <*> aopt textField (fieldWithTooltip MsgFilterCondition MsgFilterConditionTooltip) Nothing
    <*> aopt textField (fieldWithTooltip MsgTargetCondition MsgTargetConditionTooltip) Nothing
    <*> areq dayField (bfs MsgTargetDeadlineDay) Nothing
    <*> areq timeFieldTypeTime (bfs MsgTargetDeadlineTime) Nothing
    <*> areq doubleField (bfs MsgTargetValue) Nothing

indicatorForm :: Indicator -> Form Indicator
indicatorForm indicator = renderBootstrap3 BootstrapBasicForm $ Indicator
    <$> testSelectFieldList (Just $ indicatorTest indicator)
    <*> aopt textField (fieldWithTooltip MsgFilterCondition MsgFilterConditionTooltip) (Just $ indicatorFilterCondition indicator)
    <*> aopt textField (fieldWithTooltip MsgTargetCondition MsgTargetConditionTooltip) (Just $ indicatorTargetCondition indicator)

addTargetForm :: Form (Maybe Text, Day, TimeOfDay, Double)
addTargetForm = renderBootstrap3 BootstrapBasicForm $ (,,,)
    <$> aopt textField (bfs MsgTargetName) Nothing
    <*> areq dayField (bfs MsgTargetDeadlineDay) Nothing
    <*> areq timeFieldTypeTime (bfs MsgTargetDeadlineTime) Nothing
    <*> areq doubleField (bfs MsgTargetValue) Nothing

indicatorTable :: Maybe (Entity User) -> Table.Table App (IndicatorEntry)
indicatorTable mUser = mempty
  ++ Table.text "indicator" prettyIndicatorEntry
  ++ Table.text "filter condition" ((fromMaybe T.empty) . indicatorEntryFilterCondition)
  ++ Table.text "target condition" ((fromMaybe T.empty) . indicatorEntryTargetCondition)
  ++ Table.text "targets" formatTargets
  ++ indicatorStatusCell mUser

targetTable :: Maybe (Entity User) -> Maybe Int -> Table.Table App (Entity Target)
targetTable mUser mPrecision = mempty
  ++ Table.text "target name" ((fromMaybe T.empty) . targetName . entityVal)
  ++ Table.text "target value" (formatScore mPrecision . targetValue . entityVal)
  ++ timestampCell "deadline" (targetDeadline . entityVal)
  ++ targetStatusCell mUser mPrecision

prettyIndicatorEntry :: IndicatorEntry -> Text
prettyIndicatorEntry entry = prettyTestTitle (entityVal $ indicatorEntryTest entry)
                                             (entityVal $ indicatorEntryChallenge entry)

filterEntries :: Maybe Text -> [TableEntry] -> [TableEntry]
filterEntries Nothing = id
filterEntries (Just condition) = filter (\entry -> checkCondition conditionParsed (toVariantEntry entry))
  where conditionParsed = parseCondition condition
        toVariantEntry :: TableEntry -> VariantEntry
        toVariantEntry entry = VariantEntry {
          variantEntryTags = map (entityVal . fst) $ tableEntryTagsInfo entry,
          variantEntryParams = map entityVal $ tableEntryParams entry
          }

getTargetStatus :: UTCTime -> [TableEntry] -> IndicatorEntry -> Entity Target -> TargetStatus
getTargetStatus theNow entries indicator target =
  if null entries'
  then
    if theNow > (targetDeadline $ entityVal target)
      then TargetFailed
      else TargetOngoing
  else TargetPassed
  where entries' =
          filter (\v -> isBetter (testMetric $ entityVal $ indicatorEntryTest indicator)
                                v
                                (targetValue $ entityVal target))
          $ catMaybes
          $ map evaluationScore
          $ catMaybes
          $ map (\e -> (tableEntryMapping e) M.!? testId)
          $ filter (\e -> (submissionStamp $ entityVal $ tableEntrySubmission e) < theNow)
          $ filterEntries (indicatorEntryTargetCondition indicator) entries
        testId = entityKey $ indicatorEntryTest indicator

formatTargets :: IndicatorEntry -> Text
formatTargets entry = T.intercalate ", " $ (map (formatTarget (testPrecision $ entityVal $ indicatorEntryTest entry))) $ indicatorEntryTargets entry

formatTarget :: Maybe Int -> Entity Target -> Text
formatTarget mPrecision (Entity _ target) =
  (case targetName target of
     Just name -> name <> " "
     Nothing -> T.empty)
  <> (formatScore mPrecision (targetValue target))
  <> " ("
  <> (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" $ targetDeadline target)
  <> ")"

indicatorStatusCell :: Maybe (Entity User) -> Table.Table App IndicatorEntry
indicatorStatusCell mUser = Table.widget "" (indicatorStatusCellWidget mUser)

indicatorStatusCellWidget :: Maybe (Entity User) -> IndicatorEntry -> WidgetFor App ()
indicatorStatusCellWidget mUser indicatorEntry = $(widgetFile "indicator-status")
  where indicatorId = entityKey $ indicatorEntryIndicator indicatorEntry

targetStatusCell :: Maybe (Entity User) -> Maybe Int -> Table.Table App (Entity Target)
targetStatusCell mUser mPrecision = Table.widget "" (targetStatusCellWidget mUser mPrecision)

targetStatusCellWidget :: Maybe (Entity User) -> Maybe Int -> Entity Target -> WidgetFor App ()
targetStatusCellWidget mUser mPrecision targetEnt = $(widgetFile "target-status")
  where targetId = entityKey $ targetEnt

testSelectFieldList :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend, RenderMessage site AppMessage, RenderMessage site FormMessage, PersistQueryRead (YesodPersistBackend site), YesodPersist site) => Maybe TestId -> AForm (HandlerFor site) (Key Test)
testSelectFieldList mTestId = areq (selectField tests) (bfs MsgTest) mTestId
    where
      tests = do
        testEnts <- runDB $ selectList [] [Asc TestName]
        challenges <- runDB $ mapM (\(Entity _ val) -> get404 (testChallenge val)) testEnts
        let items = Import.map (\(t, ch) -> (prettyTestTitle (entityVal t) ch, entityKey t)) $ zip testEnts challenges
        optionsPairs $ sortBy (\a b -> fst a `compare` fst b) items

prettyTestTitle :: Test -> Challenge -> Text
prettyTestTitle t ch = (challengeTitle ch) ++ " / " ++ (testName t) ++ " / " ++ (pack $ show $ testMetric t)
