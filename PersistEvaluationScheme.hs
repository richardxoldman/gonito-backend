module PersistEvaluationScheme where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import GEval.EvaluationScheme
import qualified Data.Text as T

instance PersistField EvaluationScheme where
  toPersistValue m = PersistText (T.pack $ show m)

  fromPersistValue (PersistText t) = case readMay t of
    Just val -> Right val
    Nothing -> Left "Unexpected value"
  fromPersistValue _ = Left "Unexpected value"

instance PersistFieldSql EvaluationScheme where
  sqlType _ = SqlString
