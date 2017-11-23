{-# LANGUAGE TypeFamilies #-}

module Data.String.Babel where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

class (Monoid a, IsString a) => Textual a where
  toString :: a -> String
  toByteString :: a -> B.ByteString
  fromByteString :: B.ByteString -> a
  toLazyByteString :: a -> LB.ByteString
  fromLazyByteString :: LB.ByteString -> a
  toText :: a -> T.Text
  fromText :: T.Text -> a
  toLazyText :: a -> LT.Text
  fromLazyText :: LT.Text -> a

retext :: (Textual a, Textual b) => a -> b
retext = fromLazyByteString . toLazyByteString

instance (c ~ Char) => Textual [c] where
  toString = id
  toByteString = BC.pack . toString
  fromByteString = fromString . BC.unpack
  toLazyByteString = LBC.pack . toString
  fromLazyByteString = fromString . LBC.unpack
  toText = T.pack . toString
  fromText = fromString . T.unpack
  toLazyText = LT.pack . toString
  fromLazyText = fromString . LT.unpack

instance Textual T.Text where
  toString = T.unpack
  toByteString = T.encodeUtf8
  fromByteString = T.decodeUtf8
  toLazyByteString = toLazyByteString . T.encodeUtf8
  fromLazyByteString = T.decodeUtf8 . fromLazyByteString
  toText = id
  fromText = id
  toLazyText = LT.fromStrict
  fromLazyText = LT.toStrict

instance Textual LT.Text where
  toString = LT.unpack
  toByteString = toByteString . toLazyByteString
  fromByteString = toLazyText . toLazyByteString
  toLazyByteString = toLazyByteString . LT.encodeUtf8
  fromLazyByteString = LT.decodeUtf8 . fromLazyByteString
  toText = LT.toStrict
  fromText = LT.fromStrict
  toLazyText = id
  fromLazyText = id

instance Textual B.ByteString where
  toString = BC.unpack
  toByteString = id
  fromByteString = id
  toLazyByteString = LB.fromStrict
  fromLazyByteString = LB.toStrict
  toText = T.decodeUtf8
  fromText = T.encodeUtf8
  toLazyText = toLazyText . toText
  fromLazyText = fromText . fromLazyText

instance Textual LB.ByteString where
  toString = LBC.unpack
  toByteString = LB.toStrict
  fromByteString = LB.fromStrict
  toLazyByteString = id
  fromLazyByteString = id
  toText = T.decodeUtf8 . fromLazyByteString
  fromText = toLazyByteString
  toLazyText = fromLazyByteString
  fromLazyText = toLazyByteString
