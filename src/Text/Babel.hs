{-# LANGUAGE TypeFamilies #-}

-- | This is a slow, dreadful solution to The String Problem. It seems to work
--though.
--
-- Backpack could probably be used to replace this with something much better,
-- but I'm going to wait for it to mature more first.
module Text.Babel 
  (
    Textual (..)
  , showT
  , singleton
  , IsString (fromString)
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.String (IsString (fromString))
import Data.Semigroup (Semigroup)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

-- | Stringy things are 'Textual'. We can convert them to other stringy things,
-- usually slowly.
class (Semigroup a, Monoid a, IsString a) => Textual a where
  toString :: a -> String
  toByteString :: a -> B.ByteString
  fromByteString :: B.ByteString -> a
  toLazyByteString :: a -> LB.ByteString
  fromLazyByteString :: LB.ByteString -> a
  toText :: a -> T.Text
  fromText :: T.Text -> a
  toLazyText :: a -> LT.Text
  fromLazyText :: LT.Text -> a
  toByteStringBuilder :: a -> BSB.Builder
  toByteStringBuilder = toByteStringBuilder . toLazyByteString
  retext :: Textual b => a -> b

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
  retext = fromString

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
  retext = fromText

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
  retext = fromLazyText

instance Textual TB.Builder where
  toString = toString . toLazyText
  toByteString = toByteString . toLazyText
  fromByteString = TB.fromText . toText
  toLazyByteString = toLazyByteString . toLazyText
  fromLazyByteString = TB.fromLazyText . toLazyText
  toText = toText . toLazyText
  fromText = TB.fromText
  toLazyText = TB.toLazyText
  fromLazyText = TB.fromLazyText
  retext = fromLazyText . toLazyText

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
  toByteStringBuilder = BSB.byteString
  retext = fromByteString

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
  toByteStringBuilder = BSB.lazyByteString
  retext = fromLazyByteString

instance Textual BSB.Builder where
  toString = toString . toLazyByteString
  toByteString = toByteString . toLazyByteString
  fromByteString = BSB.byteString
  toLazyByteString = BSB.toLazyByteString
  fromLazyByteString = BSB.lazyByteString
  toText = toText . toLazyByteString
  fromText = fromByteString . toByteString
  toLazyText = toLazyText . toLazyByteString
  fromLazyText = fromLazyByteString . toLazyByteString
  toByteStringBuilder = id
  retext = fromLazyText . toLazyText

-- | Show to any Textual type. This necessarily goes through 'String' on the
-- way, so it won't be fast even if you pick a faster type.
showT :: (Show a, Textual t) => a -> t
showT = fromString . show

-- | Make a singleton of anything stringy
singleton :: IsString t => Char -> t
singleton = fromString . pure
