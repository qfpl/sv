{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Encode where

import Prelude 
import qualified Prelude as S (Show(..))

import Control.Lens (Getting, preview, review)
import qualified Data.Bool as B (bool)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List (intersperse)
import Data.Monoid (First, (<>))
import Data.Semigroup (Semigroup)

import Data.Sv.Config (Separator, comma)
import Text.Newline (Newline (CRLF))
import Text.Space (Spaces, spacesToString)
import Text.Quote (Quote (DoubleQuote), quoteChar)

newtype Encode a =
  Encode { getEncode :: Op [BS.Builder] a }
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

mkEncodeBuilder :: (a -> BS.Builder) -> Encode a
mkEncodeBuilder = Encode . Op . fmap pure

mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = mkEncodeBuilder . fmap BS.lazyByteString

encode :: EncodeOptions -> Encode a -> a -> LBS.ByteString
encode opts e = BS.toLazyByteString . encode' opts e

encode' :: EncodeOptions -> Encode a -> a -> BS.Builder
encode' opts e =
  let addSeparators = intersperse (BS.char8 (separator opts))
      quotep = foldMap (BS.char8 . review quoteChar) (quote opts)
      addQuotes x = quotep <> x <> quotep
      bspaces = BS.string8 . spacesToString . spacingBefore $ opts
      aspaces = BS.string8 . spacesToString . spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getOp (getEncode e)

showEncode :: S.Show a => Encode a
showEncode = mkEncodeBuilder (BS.string8 . show)

empty :: Encode a
empty = conquer

orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

string :: Encode String
string = mkEncodeBuilder BS.string8

char :: Encode Char
char = mkEncodeBuilder BS.char8

int :: Encode Int
int = mkEncodeBuilder BS.intDec

lazyByteString :: Encode LBS.ByteString
lazyByteString = mkEncodeBS id

strictByteString :: Encode Strict.ByteString
strictByteString = mkEncodeBuilder BS.byteString

byteStringBuilder :: Encode BS.Builder
byteStringBuilder = mkEncodeBuilder id

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
fromFold g x = fromFoldMay g $ orEmpty x

fromFoldMay :: Getting (First a) s a -> Encode (Maybe a) -> Encode s
fromFoldMay g x = contramap (preview g) x

data EncodeOptions =
  EncodeOptions {
    separator :: Separator
  , spacingBefore :: Spaces
  , spacingAfter :: Spaces
  , quote :: Maybe Quote
  , newline :: Newline
  , terminatingNewline :: Bool
  }

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma mempty mempty (Just DoubleQuote) CRLF True
