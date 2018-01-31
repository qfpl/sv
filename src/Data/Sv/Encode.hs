{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.Encode where

import Prelude 
import qualified Prelude as S (Show(..))

import Control.Applicative ((<**>))
import Control.Lens (Getting, preview, review)
import Control.Monad (join)
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
import Text.Escape (Escaped, getRawEscaped, Escapable (escape), escapeChar)
import Text.Newline (Newline (CRLF), newlineText)
import Text.Space (Spaces, spacesString)
import Text.Quote (Quote (DoubleQuote), quoteChar)

-- | To produce a CSV file from data types, build an 'Encode' for your data
-- type. This module contains primitives, combinators, and type class instances
-- to help you to do so.
--
-- 'Encode' is a 'Contravariant' functor, as well as a 'Divisible' and
-- 'Decidable'. 'Divisible' is the contravariant form of 'Applicative',
-- while 'Decidable' is the contravariant form of 'Control.Applicative.Alternative'.
-- These type classes will provide useful combinators for working with 'Encode's.
--
-- Specialised to 'Encode', the function 'divide' from 'Divisible' has the type:
--
-- @
-- divide :: (a -> (b,c)) -> Encode b -> Encode c -> Encode a
-- @
--
-- which can be read "if 'a' can be split into a 'b' and a 'c', and I can handle
-- 'b', and I can handle 'c', then I can handle an 'a'".
-- Here the "I can handle"
-- part corresponds to the 'Encode'. If we think of (covariant) functors as
-- being "full of" 'a', then we can think of contravariant functors as being
-- "able to handle" 'a'.
--
-- How does it work? Perform the split on the 'a', handle the 'b' by converting
-- it into some text,
-- handle the 'c' by also converting it to some text, then put each of those
-- text fragments into their own field in the CSV.
--
-- Similarly, the function 'choose' from 'Decidable', specialsed to 'Encode', has
-- the type:
--
-- @
-- choose :: (a -> Either b c) -> Encode b -> Encode c -> Encode a
-- @
--
-- which can be read "if 'a' is either 'b' or 'c', and I can handle 'b',
-- and I can handle 'c', then I can handle 'a'".
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

unsafeBuilder :: (a -> BS.Builder) -> Encode a
unsafeBuilder = Encode . pure . fmap pure

mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = unsafeBuilder . fmap BS.lazyByteString

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
      bspaces = BS.stringUtf8 . review spacesString . spacingBefore $ opts
      aspaces = BS.stringUtf8 . review spacesString . spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getEncode e opts

showEncode :: S.Show a => Encode a
showEncode = contramap show string

nop :: Encode a
nop = Encode mempty

empty :: Encode a
empty = Encode (pure (pure (pure mempty)))

orNothing :: Encode a -> Encode (Maybe a)
orNothing = choose (maybe (Left ()) Right) conquer

orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

char :: Encode Char
char = escaped' escapeChar BS.stringUtf8 BS.charUtf8 -- BS.charUtf8

int :: Encode Int
int = unsafeBuilder BS.intDec

integer :: Encode Integer
integer = unsafeBuilder BS.integerDec

float :: Encode Float
float = unsafeBuilder BS.floatDec

double :: Encode Double
double = unsafeBuilder BS.doubleDec

escaped :: Escapable s => (s -> BS.Builder) -> Encode s
escaped = join (escaped' escape)

escaped' :: (Char -> s -> Escaped t) -> (t -> BS.Builder) -> (s -> BS.Builder) -> Encode s
escaped' esc tb sb = mkEncodeWithOpts $ \opts s ->
  case quote opts of
    Nothing -> sb s
    Just q -> tb $ getRawEscaped (esc (review quoteChar q) s)

string :: Encode String
string = escaped BS.stringUtf8

text :: Encode T.Text
text = escaped (BS.byteString . T.encodeUtf8)

lazyByteString :: Encode LBS.ByteString
lazyByteString = escaped BS.lazyByteString

byteString :: Encode Strict.ByteString
byteString = escaped BS.byteString

unsafeString :: Encode String
unsafeString = unsafeBuilder BS.stringUtf8

unsafeText :: Encode T.Text
unsafeText = unsafeBuilder (BS.byteString . T.encodeUtf8)

unsafeByteStringBuilder :: Encode BS.Builder
unsafeByteStringBuilder = unsafeBuilder id

unsafeByteString :: Encode Strict.ByteString
unsafeByteString = unsafeBuilder BS.byteString

unsafeLazyByteString :: Encode LBS.ByteString
unsafeLazyByteString = unsafeBuilder BS.lazyByteString

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
