{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Sv.Encode
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module is intended to be imported qualified as follows

@import Data.Sv.Encode as E@

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

For an example of encoding, see
<https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Encoding.hs
Encoding.hs>
-}

module Data.Sv.Encode (
  Encode (Encode, getEncode)

-- * Convenience constructors
, mkEncodeWithOpts
, mkEncodeBS
, unsafeBuilder

-- * Options
, EncodeOptions (..)
, HasEncodeOptions (..)

-- * Running an Encode
, defaultEncodeOptions
, encode
, encodeToHandle
, encodeToFile
, encodeBuilder
, encodeRow
, encodeRowBuilder
, encodeSv

-- * Primitive encodes
-- ** Field-based
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
-- ** Row-based
, row

-- * Combinators
, (?>)
, (<?)
, (?>>)
, (<<?)
, encodeOf
, encodeOfMay

-- * Unsafe encodes
, unsafeString
, unsafeText
, unsafeByteString
, unsafeLazyByteString
, unsafeByteStringBuilder
, unsafeConst
) where

import qualified Prelude as P
import Prelude hiding (const)

import Control.Applicative ((<$>), (<**>))
import Control.Lens (Getting, preview, review, view)
import Control.Monad (join)
import qualified Data.Bool as B (bool)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold, foldMap, toList)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (conquer), Decidable (choose))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid (Monoid (mempty), First, (<>), mconcat)
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), viewl, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Sequence as S (singleton, empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Encode.Options (EncodeOptions (..), HasEncodeOptions (..), HasSeparator (..), defaultEncodeOptions)
import Data.Sv.Encode.Type (Encode (Encode, getEncode))
import Data.Sv.Syntax.Field (Field (Unquoted), SpacedField, unescapedField)
import Data.Sv.Syntax.Record (Record (Record), Records (EmptyRecords), emptyRecord, mkRecords, recordNel)
import Data.Sv.Syntax.Sv (Sv (Sv), Header (Header))
import qualified Data.Vector.NonEmpty as V
import Text.Escape (Escaper, Escaper', Unescaped (Unescaped), escapeChar, escapeString, escapeText, escapeUtf8, escapeUtf8Lazy)
import Text.Newline (newlineText)
import Text.Space (Spaced (Spaced), spacesString)
import Text.Quote (quoteChar)

-- | Make an 'Encode' from a function that builds one 'Field'.
mkEncodeWithOpts :: (EncodeOptions -> a -> BS.Builder) -> Encode a
mkEncodeWithOpts = Encode . (fmap (fmap pure))

-- | Make an encode from any function that returns a ByteString 'Builder'.
unsafeBuilder :: (a -> BS.Builder) -> Encode a
unsafeBuilder b = Encode (\_ a -> pure (b a))
{-# INLINE unsafeBuilder #-}

-- | Make an 'Encode' from a function that builds one 'Field'.
mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = unsafeBuilder . fmap BS.lazyByteString

-- | Encode the given list with the given 'Encode', configured by the given
-- 'EncodeOptions'.
encode :: Encode a -> EncodeOptions -> [a] -> LBS.ByteString
encode enc opts = BS.toLazyByteString . encodeBuilder enc opts

-- | Encode, writing the output to a file handle.
encodeToHandle :: Encode a -> EncodeOptions -> [a] -> Handle -> IO ()
encodeToHandle enc opts as h =
  BS.hPutBuilder h (encodeBuilder enc opts as)

-- | Encode, writing to a file. This is way is more efficient than encoding to
-- a 'ByteString' and then writing to file.
encodeToFile :: Encode a -> EncodeOptions -> [a] -> FilePath -> IO ()
encodeToFile enc opts as fp = do
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  encodeToHandle enc opts as h
  hClose h

-- | Encode to a ByteString 'Builder', which is useful if you are going
-- to combine the output with other 'ByteString's.
encodeBuilder :: Encode a -> EncodeOptions -> [a] -> BS.Builder
encodeBuilder e opts as =
  let enc = encodeRowBuilder e opts
      nl  = newlineText (_newline opts)
      terminal = if _terminalNewline opts then nl else mempty
  in  case as of
    [] -> terminal
    (a:as') -> enc a <> mconcat [nl <> enc a' | a' <- as'] <> terminal

-- | Encode one row only
encodeRow :: Encode a -> EncodeOptions -> a -> LBS.ByteString
encodeRow e opts = BS.toLazyByteString . encodeRowBuilder e opts

-- | Encode one row only, as a ByteString 'Builder'
encodeRowBuilder :: Encode a -> EncodeOptions -> a -> BS.Builder
encodeRowBuilder e opts =
  let addSeparators = intersperseSeq (BS.charUtf8 (view separator opts))
      quotep = foldMap (BS.charUtf8 . review quoteChar) (view quote opts)
      addQuotes x = quotep <> x <> quotep
      bspaces = BS.stringUtf8 . review spacesString . view spacingBefore $ opts
      aspaces = BS.stringUtf8 . review spacesString . view spacingAfter $ opts
      addSpaces x = bspaces <> x <> aspaces
  in  fold . addSeparators . fmap (addSpaces . addQuotes) . getEncode e opts

-- | Build an 'Sv' rather than going straight to 'ByteString'. This allows you
-- to query the Sv or run sanity checks.
encodeSv :: Encode a -> EncodeOptions -> Maybe (NonEmpty Strict.ByteString) -> [a] -> Sv Strict.ByteString
encodeSv e opts headerStrings as =
  let encoded :: [Seq BS.Builder]
      encoded = getEncode e opts <$> as
      nl = view newline opts
      sep = view separator opts
      mkSpaced = Spaced (_spacingBefore opts) (_spacingAfter opts)
      mkField = maybe Unquoted unescapedField (_quote opts)
      mkHeader r = Header r nl
      mkRecord :: NonEmpty z -> Record z
      mkRecord = recordNel . fmap (mkSpaced . mkField)
      header :: Maybe (Header Strict.ByteString)
      header = mkHeader . mkRecord <$> headerStrings
      rs :: Records Strict.ByteString
      rs = l2rs (b2r <$> encoded)
      l2rs = maybe EmptyRecords (mkRecords nl) . nonEmpty -- Records . fmap (skrinple nl) . nonEmpty
      terminal = if _terminalNewline opts then [nl] else []
      b2f :: BS.Builder -> SpacedField Strict.ByteString
      b2f = mkSpaced . mkField . LBS.toStrict . BS.toLazyByteString
      b2r :: Seq BS.Builder -> Record Strict.ByteString
      b2r = maybe emptyRecord (Record . V.fromNel) . nonEmpty . toList . fmap b2f
  in  Sv sep header rs terminal

-- | Encode this 'Data.ByteString.ByteString' every time, ignoring the input.
const :: Strict.ByteString -> Encode a
const b = contramap (P.const b) byteString

-- | Build an 'Encode' using a type's 'Show' instance.
showEncode :: Show a => Encode a
showEncode = contramap show string

-- | Don't encode anything.
nop :: Encode a
nop = conquer

-- | Encode anything as the empty string.
empty :: Encode a
empty = Encode (pure (pure (pure mempty)))

-- | Lift an Encode to be able to hanlde 'Maybe', by using the empty string
-- in the case of 'Nothing'
orEmpty :: Encode a -> Encode (Maybe a)
orEmpty = choose (maybe (Left ()) Right) empty

-- | Build an 'Encode' for 'Maybe' given a 'Just' and a 'Nothing' encode.
(?>) :: Encode a -> Encode () -> Encode (Maybe a)
(?>) = flip (<?)
{-# INLINE (?>) #-}

-- | Build an 'Encode' for 'Maybe' given a 'Nothing' and a 'Just' encode.
(<?) :: Encode () -> Encode a -> Encode (Maybe a)
(<?) = choose (maybe (Left ()) Right)
{-# INLINE (<?) #-}

-- | Build an 'Encode' for 'Maybe' given a 'Just' encode and a
-- 'Data.ByteString.Strict.ByteString' for the 'Nothing' case.
(?>>) :: Encode a -> Strict.ByteString -> Encode (Maybe a)
(?>>) a s = a ?> const s
{-# INLINE (?>>) #-}

-- | Build an 'Encode' for 'Maybe' given a  'Data.ByteString.Strict.ByteString'
-- for the 'Nothing' case and a 'Just' encode.
(<<?) :: Strict.ByteString -> Encode a -> Encode (Maybe a)
(<<?) = flip (?>>)
{-# INLINE (<<?) #-}

-- | Encode a list as a whole row at once, using the same 'Encode'
-- for every element
row :: Encode s -> Encode [s]
row enc = Encode $ \opts list -> join $ Seq.fromList $ fmap (getEncode enc opts) list

-- | Encode a single 'Char'
char :: Encode Char
char = escaped escapeChar BS.charUtf8 BS.stringUtf8

-- | Encode an 'Int'
int :: Encode Int
int = unsafeBuilder BS.intDec

-- | Encode an 'Integer'
integer :: Encode Integer
integer = unsafeBuilder BS.integerDec

-- | Encode a 'Float'
float :: Encode Float
float = unsafeBuilder BS.floatDec

-- | Encode a 'Double'
double :: Encode Double
double = unsafeBuilder BS.doubleDec

-- | Encode a 'String'
string :: Encode String
string = escaped' escapeString BS.stringUtf8

-- | Encode a 'Data.Text.Text'
text :: Encode T.Text
text = escaped' escapeText (BS.byteString . T.encodeUtf8)

-- | Encode a strict 'Data.ByteString.ByteString'
byteString :: Encode Strict.ByteString
byteString = escaped' escapeUtf8 BS.byteString

-- | Encode a lazy 'Data.ByteString.Lazy.ByteString'
lazyByteString :: Encode LBS.ByteString
lazyByteString = escaped' escapeUtf8Lazy BS.lazyByteString

escaped :: Escaper s t -> (s -> BS.Builder) -> (t -> BS.Builder) -> Encode s
escaped esc sb tb = mkEncodeWithOpts $ \opts s ->
  case _quote opts of
    Nothing -> sb s
    Just q -> tb $ esc (review quoteChar q) (Unescaped s)

escaped' :: Escaper' s -> (s -> BS.Builder) -> Encode s
escaped' escaper = join (escaped escaper)

-- | Encode a 'Bool' as False or True
boolTrueFalse :: Encode Bool
boolTrueFalse = mkEncodeBS $ B.bool "False" "True"

-- | Encode a 'Bool' as false or true
booltruefalse :: Encode Bool
booltruefalse = mkEncodeBS $ B.bool "false" "true"

-- | Encode a 'Bool' as no or yes
boolyesno :: Encode Bool
boolyesno = mkEncodeBS $ B.bool "no" "yes"

-- | Encode a 'Bool' as No or Yes
boolYesNo :: Encode Bool
boolYesNo = mkEncodeBS $ B.bool "No" "Yes"

-- | Encode a 'Bool' as N or Y
boolYN :: Encode Bool
boolYN = mkEncodeBS $ B.bool "N" "Y"

-- | Encode a 'Bool' as 0 or 1
bool10 :: Encode Bool
bool10 = mkEncodeBS $ B.bool "0" "1"

-- | Given an optic from @s@ to @a@, Try to use it to build an encode.
--
-- @
-- encodeOf :: Iso'       s a -> Encode a -> Encode s
-- encodeOf :: Lens'      s a -> Encode a -> Encode s
-- encodeOf :: Prism'     s a -> Encode a -> Encode s
-- encodeOf :: Traversal' s a -> Encode a -> Encode s
-- encodeOf :: Fold       s a -> Encode a -> Encode s
-- encodeOf :: Getter     s a -> Encode a -> Encode s
-- @
--
-- This is very useful when you have a prism for each constructor of your type.
-- You can define an 'Encode' as follows:
--
-- @
-- myEitherEncode :: Encode a -> Encode b -> Encode (Either a b)
-- myEitherEncode encA encB = encodeOf _Left encA <> encodeOf _Right encB
-- @
--
-- In this example, when the prism lookup returns 'Nothing', the empty encoder
-- is returned. This is the 'mempty' for the 'Encode' monoid, so it won't
-- add a field to the resulting CSV. This is the behaviour you want for
-- combining a collection of prisms.
--
-- But this encoder also works with lenses (or weaker optics), which will
-- never fail their lookup, in which case it never returns 'mempty'.
-- So this actually does the right thing for both sum and product types.
encodeOf :: Getting (First a) s a -> Encode a -> Encode s
encodeOf g = encodeOfMay g . choose (maybe (Left ()) Right) conquer

-- | Like 'encodeOf', but you can handle 'Nothing' however you'd like.
-- In 'encodeOf', it is handled by the Encode which does nothing,
-- but for example you might like to use 'orEmpty' to encode an empty field.
encodeOfMay :: Getting (First a) s a -> Encode (Maybe a) -> Encode s
encodeOfMay g x = contramap (preview g) x

-- | Encode a 'String' really quickly.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeString :: Encode String
unsafeString = unsafeBuilder BS.stringUtf8

-- | Encode 'Text' really quickly.
-- If the text has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeText :: Encode T.Text
unsafeText = unsafeBuilder (BS.byteString . T.encodeUtf8)

-- | Encode ByteString 'Builder' really quickly.
-- If the builder builds a string with quotes in it, they will not be escaped
-- properly, so the result maybe not be valid CSV
unsafeByteStringBuilder :: Encode BS.Builder
unsafeByteStringBuilder = unsafeBuilder id

-- | Encode a 'Data.ByteString.ByteString' really quickly.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeByteString :: Encode Strict.ByteString
unsafeByteString = unsafeBuilder BS.byteString

-- | Encode a 'Data.ByteString.Lazy.ByteString' really quickly.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeLazyByteString :: Encode LBS.ByteString
unsafeLazyByteString = unsafeBuilder BS.lazyByteString

-- | Encode this 'ByteString' really quickly every time, ignoring the input.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeConst :: Strict.ByteString -> Encode a
unsafeConst b = contramap (P.const b) unsafeByteString

-- Added in containers 0.5.8, but we duplicate it here to support older GHCs
intersperseSeq :: a -> Seq a -> Seq a
intersperseSeq y xs = case viewl xs of
  EmptyL -> S.empty
  p :< ps -> p <| (ps <**> (P.const y <| S.singleton id))
