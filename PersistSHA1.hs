module PersistSHA1 where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Numeric (showHex)

data SHA1 = SHA1 ByteString
            deriving Show

toHex :: ByteString -> ByteString
toHex = BC.pack . concat . (map ("\\x"++)) . (map (flip showHex "")) . B.unpack

toSHA1 :: ByteString -> SHA1
toSHA1 x = SHA1 $ B.concat ["E'\\\\x", x, "'"]

fromTextToSHA1 :: Text -> SHA1
fromTextToSHA1 = SHA1 . B.pack . (map hexByteToWord8) . (T.chunksOf 2)

hexByteToWord8 :: Text -> Word8
hexByteToWord8 t = (hexNibbleToWord8 $ T.head t) * 16 + (hexNibbleToWord8 $ T.index t 1)

hexNibbleToWord8 :: Char -> Word8
hexNibbleToWord8 '0' = 0
hexNibbleToWord8 '1' = 1
hexNibbleToWord8 '2' = 2
hexNibbleToWord8 '3' = 3
hexNibbleToWord8 '4' = 4
hexNibbleToWord8 '5' = 5
hexNibbleToWord8 '6' = 6
hexNibbleToWord8 '7' = 7
hexNibbleToWord8 '8' = 8
hexNibbleToWord8 '9' = 9
hexNibbleToWord8 'A' = 10
hexNibbleToWord8 'a' = 10
hexNibbleToWord8 'B' = 11
hexNibbleToWord8 'b' = 11
hexNibbleToWord8 'C' = 12
hexNibbleToWord8 'c' = 12
hexNibbleToWord8 'D' = 13
hexNibbleToWord8 'd' = 13
hexNibbleToWord8 'E' = 14
hexNibbleToWord8 'e' = 14
hexNibbleToWord8 'F' = 15
hexNibbleToWord8 'f' = 15


instance PersistField SHA1 where
  toPersistValue (SHA1 t) = PersistByteString t

  fromPersistValue (PersistByteString t) = Right $ SHA1 t
  fromPersistValue _ = Left "Unexpected value"

instance PersistFieldSql SHA1 where
  sqlType _ = SqlBlob
