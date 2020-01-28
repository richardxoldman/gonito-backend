{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses, PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls #-}

import Prelude hiding (FilePath)

import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Model
--share [mkPersist sqlSettings, mkMigrate "migrateAll"]
--    $(persistFileWith lowerCaseSettings "config/models")

dbSpecification dbName = BS.concat ["host=localhost dbname=", BSU.fromString dbName]

dbConnection dbName = withPostgresqlConn (dbSpecification dbName)

runOnDb :: String -> ReaderT SqlBackend _ a -> IO a
runOnDb dbName = runNoLoggingT . runResourceT . (dbConnection dbName) . runSqlConn

process :: String -> IO ()
process dbName = do
  Prelude.putStrLn "Getting all variants…"
  variants <- runOnDb dbName
             $ E.select $ E.from $ \(challenge, submission, variant, test, out) -> do
                          E.where_ (submission ^. SubmissionChallenge E.==. challenge ^. ChallengeId
                                    E.&&. variant ^. VariantSubmission E.==. submission ^. SubmissionId
                                    E.&&. test ^. TestChallenge E.==. challenge ^. ChallengeId
                                    E.&&. out ^. OutTest E.==. test ^. TestId
                                    E.&&. out ^. OutVariant E.==. variant ^. VariantId)
                          return (variant, submission, out, test)

  Prelude.putStrLn "Adding evaluations…"
  _ <- mapM (processVariant dbName) variants

  putStrLn "DELETING"
  runOnDb dbName $ deleteWhere [EvaluationVersion ==. Nothing]

  return ()

processVariant :: String -> (Entity Variant, Entity Submission, Entity Out, Entity Test) -> IO ()
processVariant dbName (variant, Entity _ submission, Entity _ out, Entity testId _) = do
  Prelude.putStrLn (show $ entityKey variant)

  evaluations <- runOnDb dbName
                $ E.select $ E.from $ \evaluation -> do
                             E.where_ (E.val (outChecksum out) E.==. evaluation ^. EvaluationChecksum
                                       -- all this complication here and with orderBy due
                                       -- to the legacy issue with evaluation version sometimes missing
                                       E.&&. (evaluation ^. EvaluationVersion E.==. E.just (E.val (submissionVersion submission))
                                              E.||. E.isNothing (evaluation ^. EvaluationVersion))
                                       E.&&. evaluation ^. EvaluationTest E.==. E.val testId)
                             E.orderBy [E.desc (E.isNothing (evaluation ^. EvaluationVersion))]
                             return evaluation

  case evaluations of
    (Entity _ e:_) -> do
      case evaluationVersion e of
        Just _ -> putStrLn "OK found!"
        Nothing -> do
          putStrLn "NONE FOUND INSERTING"
          _ <- runOnDb dbName $ insert $ Evaluation testId
                                                   (outChecksum out)
                                                   (evaluationScore e)
                                                   (evaluationErrorBound e)
                                                   (evaluationErrorMessage e)
                                                   (evaluationStamp e)
                                                   (Just $ submissionVersion submission)
          return ()
    [] -> do
      putStrLn "MISSING EVALUATION"

  return ()

main :: IO ()
main = do
  let dbName = "gonito"
  process dbName
