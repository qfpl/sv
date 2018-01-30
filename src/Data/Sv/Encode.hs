{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Encode where

import Prelude 
import qualified Prelude as S (Show(..))

import Control.Applicative ((<**>))
import Control.Lens (Getting, preview, review)
import Data.Bifoldable (bifoldMap)
import qualified Data.Bool as B (bool)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid (First, (<>))
import Data.Semigroup (Semigroup)
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as S (singleton, empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (absurd)

import Data.Sv.Config (Separator, comma)
import Text.Escaped (escapeString, escapeText, escapeUtf8, escapeLazyUtf8)
import Text.Newline (Newline (CRLF), newlineText)
import Text.Space (Spaces, spacesToString)
import Text.Quote (Quote (DoubleQuote), quoteChar)

newtype Encode a =
  Encode { getEncode :: EncodeOptions -> a -> Seq BS.Builder }
  deriving (Semigroup, Monoid)

instance Contravariant Encode where
  contramap f (Encode g) = Encode $ fmap (. f) g

instance Divisible Encode where
  conquer = Encode mempty
  divide f (Encode x) (Encode y) =
    Encode $ \e a -> bifoldMap (x e) (y e) (f a)

instance Decidable Encode where
  lose f = Encode (const (absurd . f))
  choose f (Encode x) (Encode y) =
    Encode $ \e a -> either (x e) (y e) (f a)

mkEncodeWithOpts :: (EncodeOptions -> a -> BS.Builder) -> Encode a
mkEncodeWithOpts = Encode . (fmap (fmap pure))

builder :: (a -> BS.Builder) -> Encode a
builder = Encode . pure . fmap pure

mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = builder . fmap BS.lazyByteString

encode :: EncodeOptions -> Encode a -> [a] -> LBS.ByteString
encode opts enc = BS.toLazyByteString . encode' opts enc

encode' :: EncodeOptions -> Encode a -> [a] -> BS.Builder
encode' opts e as =
  let enc = encodeRow' opts e
      nl  = newlineText (newline opts)
      terminal = if terminalNewline opts then nl else mempty
  in  case as of
    [] -> terminal
    (a:as') -> enc a <> mconcat [nl <> enc a' | a' <- as'] <> terminal

encodeRow :: EncodeOptions -> Encode a -> a -> LBS.ByteString
encodeRow opts e = BS.toLazyByteString . encodeRow' opts e

encodeRow' :: EncodeOptions -> Encode a -> a -> BS.Builder
encodeRow' opts e =
  let addSeparators = intersperseSeq (BS.charUtf8 (separator opts))
      quotep = foldMap (BS.charUtf8 . review quoteChar) (quote opts)
      addQuotes x = quotep <> x <> quotep
      bspaces = BS.stringUtf8 . spacesToString . spacingBefore $ opts
      aspaces = BS.stringUtf8 . spacesToString . spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getEncode e opts

showEncode :: S.Show a => Encode a
showEncode = builder (BS.stringUtf8 . show)

nop :: Encode a
nop = Encode mempty

empty :: Encode a
empty = Encode (pure (pure (pure mempty)))

orNothing :: Encode a -> Encode (Maybe a)
orNothing = choose (maybe (Left ()) Right) conquer

orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

char :: Encode Char
char = builder BS.charUtf8

int :: Encode Int
int = builder BS.intDec

integer :: Encode Integer
integer = builder BS.integerDec

float :: Encode Float
float = builder BS.floatDec

double :: Encode Double
double = builder BS.doubleDec

escaped :: (s -> BS.Builder) -> (Char -> s -> s) -> Encode s
escaped b escape = mkEncodeWithOpts $ \opts s ->
  b $ case quote opts of
    Nothing -> s
    Just q -> escape (review quoteChar q) s

string :: Encode String
string = escaped BS.stringUtf8 escapeString

text :: Encode T.Text
text = escaped (BS.byteString . T.encodeUtf8) escapeText

lazyByteString :: Encode LBS.ByteString
lazyByteString = escaped BS.lazyByteString escapeLazyUtf8

byteString :: Encode Strict.ByteString
byteString = escaped BS.byteString escapeUtf8

unsafeString :: Encode String
unsafeString = builder BS.stringUtf8

unsafeText :: Encode T.Text
unsafeText = builder (BS.byteString . T.encodeUtf8)

unsafeByteStringBuilder :: Encode BS.Builder
unsafeByteStringBuilder = builder id

unsafeByteString :: Encode Strict.ByteString
unsafeByteString = builder BS.byteString

unsafeLazyByteString :: Encode LBS.ByteString
unsafeLazyByteString = builder BS.lazyByteString

boolTrueFalse :: Encode Bool
boolTrueFalse = mkEncodeBS $ B.bool "False" "True"

booltruefalse :: Encode Bool
booltruefalse = mkEncodeBS $ B.bool "false" "true"

boolyesno :: Encode Bool
boolyesno = mkEncodeBS $ B.bool "no" "yes"

boolYesNo :: Encode Bool
boolYesNo = mkEncodeBS $ B.bool "No" "Yes"

boolYN :: Encode Bool
boolYN = mkEncodeBS $ B.bool "N" "Y"

bool10 :: Encode Bool
bool10 = mkEncodeBS $ B.bool "0" "1"

fromFold :: Getting (First a) s a -> Encode a -> Encode s
fromFold g x = fromFoldMay g $ orNothing x

fromFoldMay :: Getting (First a) s a -> Encode (Maybe a) -> Encode s
fromFoldMay g x = contramap (preview g) x

data EncodeOptions =
  EncodeOptions {
    separator :: Separator
  , spacingBefore :: Spaces
  , spacingAfter :: Spaces
  , quote :: Maybe Quote
  , newline :: Newline
  , terminalNewline :: Bool
  }

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma mempty mempty (Just DoubleQuote) CRLF False

-- Added in containers 0.5.8, but we duplicate it here to support older GHCs
intersperseSeq :: a -> Seq a -> Seq a
intersperseSeq y xs = case viewl xs of
  EmptyL -> S.empty
  p :< ps -> p <| (ps <**> (const y <| S.singleton id))
