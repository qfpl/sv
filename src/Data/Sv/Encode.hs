{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Encode where

import Prelude 
import qualified Prelude as S (Show(..))

import Control.Lens (Getting, preview, review)
import qualified Data.Bool as B (bool)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List (intersperse)
import Data.Monoid (First)
import Data.Semigroup (Semigroup ((<>)))
import qualified Text.Show.ByteString as BS

import Text.Newline (Newline (CRLF))
import Text.Space (Spaces, spacesToString)
import Text.Quote (Quote (DoubleQuote), quoteChar)

newtype Encode a =
  Encode { getEncode :: Op [BS.Put] a }
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

mkEncode :: (a -> BS.Put) -> Encode a
mkEncode = Encode . Op . fmap pure

mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS f = mkEncode (putLazyByteString . f)

encode :: EncodeOptions -> Encode a -> a -> LBS.ByteString
encode opts e = runPut . encode' opts e

encode' :: EncodeOptions -> Encode a -> a -> Put
encode' opts e =
  let addSeparators = intersperse (putCharUtf8 (separator opts))
      quotep = foldMap (putCharUtf8 . review quoteChar) (quote opts)
      addQuotes x = quotep <> x <> quotep
      bspaces = putStringUtf8 . spacesToString . spacingBefore $ opts
      aspaces = putStringUtf8 . spacesToString . spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getOp (getEncode e)

showEncode :: S.Show a => Encode a
showEncode = mkEncode (putStringUtf8 . show)

showEncodeBS :: BS.Show a => Encode a
showEncodeBS = mkEncode BS.showp

empty :: Encode a
empty = conquer

orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

string :: Encode String
string = mkEncode putStringUtf8

char :: Encode Char
char = mkEncode putCharUtf8

int :: Encode Int
int = showEncodeBS

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
    separator :: Char
  , spacingBefore :: Spaces
  , spacingAfter :: Spaces
  , quote :: Maybe Quote
  , newline :: Newline
  , terminatingNewline :: Bool
  }

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions ',' mempty mempty (Just DoubleQuote) CRLF True
