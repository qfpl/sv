{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Sv.Encode
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

To produce a CSV file from data types, build an 'Encode' for your data
type. This module contains primitives, combinators, and type class instances
to help you to do so.

'Encode' is a 'Contravariant' functor, as well as a 'Divisible' and
'Decidable'. 'Divisible' is the contravariant form of 'Applicative',
while 'Decidable' is the contravariant form of 'Control.Applicative.Alternative'.
These type classes will provide useful combinators for working with 'Encode's.

Specialised to 'Encode', the function 'divide' from 'Divisible' has the type:

@
divide :: (a -> (b,c)) -> Encode b -> Encode c -> Encode a
@

which can be read "if 'a' can be split into a 'b' and a 'c', and I can handle
'b', and I can handle 'c', then I can handle an 'a'".
Here the "I can handle"
part corresponds to the 'Encode'. If we think of (covariant) functors as
being "full of" 'a', then we can think of contravariant functors as being
"able to handle" 'a'.

How does it work? Perform the split on the 'a', handle the 'b' by converting
it into some text,
handle the 'c' by also converting it to some text, then put each of those
text fragments into their own field in the CSV.

Similarly, the function 'choose' from 'Decidable', specialsed to 'Encode', has
the type:

@
choose :: (a -> Either b c) -> Encode b -> Encode c -> Encode a
@

which can be read "if 'a' is either 'b' or 'c', and I can handle 'b',
and I can handle 'c', then I can handle 'a'".
-}

module Data.Sv.Encode (
  Encode (Encode, getEncode)

-- * Convenience constructors
, mkEncodeWithOpts
, mkEncodeBS
, unsafeBuilder

-- * Options
, EncodeOptions (EncodeOptions, _separator, _spacingBefore, _spacingAfter, _quote, _newline, _terminalNewline)
, HasEncodeOptions (encodeOptions, separator, spacingBefore, spacingAfter, quote, newline, terminalNewline)

-- * Running an Encode
, defaultEncodeOptions
, encode
, encode'
, encodeRow
, encodeRow'
, encodeSv

-- * Primitive encodes
, const
, showEncode
, nop
, empty
, orEmpty
, char
, int
, integer
, float
, double
, boolTrueFalse
, booltruefalse
, boolyesno
, boolYesNo
, boolYN
, bool10
, string
, text
, byteString
, lazyByteString
, unsafeString
, unsafeText
, unsafeByteString
, unsafeLazyByteString
, unsafeByteStringBuilder

-- * Combinators
, divide
, conquer
, choose
, lose
, (?>)
, (<?)
, (?>>)
, (<<?)
, fromFold
, fromFoldMay
) where

import qualified Prelude as P
import Prelude hiding (const)

import Control.Applicative ((<$>), (<**>))
import Control.Lens (Getting, Lens', preview, review)
import Control.Monad (join)
import Data.Bifoldable (bifoldMap)
import qualified Data.Bool as B (bool)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold, foldMap, toList)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid (Monoid (mempty), First, (<>), mconcat)
import Data.Semigroup (Semigroup)
import Data.Separated (skrinple)
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as S (singleton, empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (absurd)

import Data.Sv.Parser.Options (Separator, comma)
import Data.Sv.Sv (Sv (Sv), Header (Header))
import Data.Sv.Field (Field (Unquoted), SpacedField, unescapedField)
import Data.Sv.Record (Record (Record), Records (Records), emptyRecord)
import Text.Babel (toByteString)
import Text.Escape (Escaped, getRawEscaped, Escapable (escape), escapeChar)
import Text.Newline (Newline (CRLF), newlineText)
import Text.Space (Spaces, Spaced (Spaced), spacesString)
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
  lose f = Encode (P.const (absurd . f))
  choose f (Encode x) (Encode y) =
    Encode $ \e a -> either (x e) (y e) (f a)

mkEncodeWithOpts :: (EncodeOptions -> a -> BS.Builder) -> Encode a
mkEncodeWithOpts = Encode . (fmap (fmap pure))

unsafeBuilder :: (a -> BS.Builder) -> Encode a
unsafeBuilder b = Encode (\_ a -> pure (b a))
{-# INLINE unsafeBuilder #-}

mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = unsafeBuilder . fmap BS.lazyByteString

encode :: EncodeOptions -> Encode a -> [a] -> LBS.ByteString
encode opts enc = BS.toLazyByteString . encode' opts enc

encode' :: EncodeOptions -> Encode a -> [a] -> BS.Builder
encode' opts e as =
  let enc = encodeRow' opts e
      nl  = newlineText (_newline opts)
      terminal = if _terminalNewline opts then nl else mempty
  in  case as of
    [] -> terminal
    (a:as') -> enc a <> mconcat [nl <> enc a' | a' <- as'] <> terminal

encodeRow :: EncodeOptions -> Encode a -> a -> LBS.ByteString
encodeRow opts e = BS.toLazyByteString . encodeRow' opts e

encodeRow' :: EncodeOptions -> Encode a -> a -> BS.Builder
encodeRow' opts e =
  let addSeparators = intersperseSeq (BS.charUtf8 (_separator opts))
      quotep = foldMap (BS.charUtf8 . review quoteChar) (_quote opts)
      addQuotes x = quotep <> x <> quotep
      bspaces = BS.stringUtf8 . review spacesString . _spacingBefore $ opts
      aspaces = BS.stringUtf8 . review spacesString . _spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getEncode e opts

encodeSv :: forall s a . Escapable s => EncodeOptions -> Encode a -> Maybe (NonEmpty s) -> [a] -> Sv Strict.ByteString
encodeSv opts e headerStrings as =
  let encoded :: [Seq BS.Builder]
      encoded = getEncode e opts <$> as
      nl = _newline opts
      sep = _separator opts
      mkSpaced = Spaced (_spacingBefore opts) (_spacingAfter opts)
      mkField = maybe Unquoted unescapedField (_quote opts)
      mkHeader r = Header r nl
      mkRecord :: NonEmpty z -> Record z
      mkRecord = Record . fmap (mkSpaced . mkField)
      header :: Maybe (Header Strict.ByteString)
      header = mkHeader . mkRecord . fmap toByteString <$> headerStrings
      rs :: Records Strict.ByteString
      rs = l2rs (b2r <$> encoded)
      l2rs = Records . fmap (skrinple nl) . nonEmpty
      terminal = if _terminalNewline opts then [nl] else []
      b2f :: BS.Builder -> SpacedField Strict.ByteString
      b2f = mkSpaced . mkField . LBS.toStrict . BS.toLazyByteString
      b2r :: Seq BS.Builder -> Record Strict.ByteString
      b2r = maybe emptyRecord Record . nonEmpty . toList . fmap b2f
  in  Sv sep header rs terminal

const :: Strict.ByteString -> Encode a
const = Encode . pure . pure . pure . BS.byteString

showEncode :: Show a => Encode a
showEncode = contramap show string

nop :: Encode a
nop = conquer

empty :: Encode a
empty = Encode (pure (pure (pure mempty)))

orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

(?>) :: Encode a -> Encode () -> Encode (Maybe a)
(?>) = flip (<?)
{-# INLINE (?>) #-}

(<?) :: Encode () -> Encode a -> Encode (Maybe a)
(<?) = choose (maybe (Left ()) Right)
{-# INLINE (<?) #-}

(?>>) :: Encode a -> Strict.ByteString -> Encode (Maybe a)
(?>>) a s = a ?> const s
{-# INLINE (?>>) #-}

(<<?) :: Strict.ByteString -> Encode a -> Encode (Maybe a)
(<<?) = flip (?>>)
{-# INLINE (<<?) #-}

char :: Encode Char
char = escaped' escapeChar BS.stringUtf8 BS.charUtf8

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
  case _quote opts of
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
fromFold g = fromFoldMay g . choose (maybe (Left ()) Right) conquer

fromFoldMay :: Getting (First a) s a -> Encode (Maybe a) -> Encode s
fromFoldMay g x = contramap (preview g) x

data EncodeOptions =
  EncodeOptions {
    _separator :: Separator
  , _spacingBefore :: Spaces
  , _spacingAfter :: Spaces
  , _quote :: Maybe Quote
  , _newline :: Newline
  , _terminalNewline :: Bool
  }

class HasEncodeOptions c where
  encodeOptions :: Lens' c EncodeOptions
  newline :: Lens' c Newline
  {-# INLINE newline #-}
  quote :: Lens' c (Maybe Quote)
  {-# INLINE quote #-}
  separator :: Lens' c Separator
  {-# INLINE separator #-}
  spacingAfter :: Lens' c Spaces
  {-# INLINE spacingAfter #-}
  spacingBefore :: Lens' c Spaces
  {-# INLINE spacingBefore #-}
  terminalNewline :: Lens' c Bool
  {-# INLINE terminalNewline #-}
  newline = encodeOptions . newline
  quote = encodeOptions . quote
  separator = encodeOptions . separator
  spacingAfter = encodeOptions . spacingAfter
  spacingBefore = encodeOptions . spacingBefore
  terminalNewline = encodeOptions . terminalNewline

instance HasEncodeOptions EncodeOptions where
  {-# INLINE newline #-}
  {-# INLINE quote #-}
  {-# INLINE separator #-}
  {-# INLINE spacingAfter #-}
  {-# INLINE spacingBefore #-}
  {-# INLINE terminalNewline #-}
  encodeOptions = id
  newline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 y x6) (f x5)
  quote f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 y x5 x6) (f x4)
  separator f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions y x2 x3 x4 x5 x6) (f x1)
  spacingAfter f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 y x4 x5 x6) (f x3)
  spacingBefore f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 y x3 x4 x5 x6) (f x2)
  terminalNewline f (EncodeOptions x1 x2 x3 x4 x5 x6) =
    fmap (\ y -> EncodeOptions x1 x2 x3 x4 x5 y) (f x6)

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions comma mempty mempty (Just DoubleQuote) CRLF False

-- Added in containers 0.5.8, but we duplicate it here to support older GHCs
intersperseSeq :: a -> Seq a -> Seq a
intersperseSeq y xs = case viewl xs of
  EmptyL -> S.empty
  p :< ps -> p <| (ps <**> (P.const y <| S.singleton id))
