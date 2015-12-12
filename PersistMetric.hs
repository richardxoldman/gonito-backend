module PersistMetric where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import GEval.Core
import qualified Data.Text as T

instance PersistField Metric where
  toPersistValue m = PersistText (T.pack $ show m)

  fromPersistValue (PersistText t) = case readMay t of
    Just val -> Right val
    Nothing -> Left "Unexpected value"
  fromPersistValue _ = Left "Unexpected value"

instance PersistFieldSql Metric where
  sqlType _ = SqlString
