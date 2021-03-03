module PersistTeamActionType where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import qualified Data.Text as T

data TeamActionType = TeamCreation | TeamJoining | TeamLeaving
  deriving (Eq, Show, Read)

instance PersistField TeamActionType where
  toPersistValue m = PersistText (T.pack $ show m)

  fromPersistValue (PersistText t) = case readMay t of
    Just val -> Right val
    Nothing -> Left ("Unexpected value '" ++ t ++ "'")
  fromPersistValue _ = Left "Unexpected value"

instance PersistFieldSql TeamActionType where
  sqlType _ = SqlString
