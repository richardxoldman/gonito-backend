module PersistSHA1 where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Numeric (showHex)

data SHA1 = SHA1 ByteString
            deriving Show

toHex :: ByteString -> ByteString
toHex = BC.pack . concat . (map (flip showHex "")) . B.unpack

instance PersistField SHA1 where
  toPersistValue (SHA1 t) = PersistDbSpecific t

  fromPersistValue (PersistDbSpecific t) = Right $ SHA1 $ B.concat ["E'\\x", toHex(t), "'"]
  fromPersistValue _ = Left "SHA1 values must be converted from PersistDbSpecific"

instance PersistFieldSql SHA1 where
  sqlType _ = SqlOther "BYTEA"
