{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Data.Sv.Encode.Core
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module is intended to be imported qualified as follows

@import Data.Sv.Encode.Core as E@

To produce a CSV file from data types, build an 'Encode' for your data
type. This module contains primitives, combinators, and type class instances
to help you to do so.

'Encode' is a 'Contravariant' functor, as well as a 'Divisible' and
'Decidable'. 'Divisible' is the contravariant form of 'Applicative',
while 'Decidable' is the contravariant form of 'Control.Applicative.Alternative'.
These type classes will provide useful combinators for working with 'Encode's.

Specialised to 'Encode', the function 'Data.Functor.Contravariant.Divisible.divide'
from 'Divisible' has the type:

@
divide :: (a -> (b,c)) -> Encode b -> Encode c -> Encode a
@

which can be read "if 'a' can be split into 'b' and 'c', and I can handle
'b', and I can handle 'c', then I can handle 'a'".

Here the "I can handle"
part corresponds to the 'Encode'. If we think of (covariant) functors as
being "full of" 'a', then we can think of contravariant functors as being
"able to handle" 'a'.

How does it work? Perform the split on the 'a', handle the 'b' by converting
it into some text,
handle the 'c' by also converting it to some text, then put each of those
text fragments into their own field in the CSV.

Similarly, the function 'Data.Functor.Contravariant.Divisible.choose'
from 'Decidable', specialsed to 'Encode', has the type:

@
choose :: (a -> Either b c) -> Encode b -> Encode c -> Encode a
@

which can be read "if 'a' is either 'b' or 'c', and I can handle 'b',
and I can handle 'c', then I can handle 'a'".

This works by performing the split, then checking whether 'b' or 'c' resulted,
then using the appropriate 'Encode'.

For an example of encoding, see
<https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Encoding.hs Encoding.hs>
-}

module Data.Sv.Encode.Core (
  Encode (..)

-- * Convenience constructors
, mkEncodeBS
, mkEncodeWithOpts

-- * Running an Encode
, encode
, encodeNamed
, encodeToHandle
, encodeNamedToHandle
, encodeToFile
, encodeNamedToFile
, encodeBuilder
, encodeNamedBuilder
, encodeRow
, encodeRowBuilder

-- * Options
, module Data.Sv.Encode.Options

-- * Primitive encodes
-- ** Name-based
, named
, (=:)
, unNameEncode
-- ** Field-based
, const
, show
, nop
, empty
, orEmpty
, char
, int
, integer
, float
, double
, doubleFast
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
, unsafeBuilder
, unsafeString
, unsafeText
, unsafeByteString
, unsafeLazyByteString
, unsafeByteStringBuilder
, unsafeConst
) where

import qualified Prelude as P
import Prelude hiding (const, show)

import Control.Lens (Getting, preview, view, _1)
import Control.Monad (join)
import Control.Monad.Writer (runWriter, writer)
import qualified Data.Bool as B (bool)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Double.Conversion.ByteString as DC
import Data.Foldable (fold)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Compose (ComposeFC (ComposeFC, getComposeFC))
import Data.Functor.Contravariant.Divisible (Divisible (conquer), Decidable (choose))
import Data.Monoid (Monoid (mempty), First, (<>), mconcat)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Word (Word8)
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hSetBinaryMode, hSetBuffering, openFile, IOMode (WriteMode))

import Data.Sv.Alien.Containers (intersperseSeq)
import Data.Sv.Encode.Options (EncodeOptions (EncodeOptions, _encodeSeparator, _newline, _terminalNewline, _quoting), HasEncodeOptions (), HasSeparator (separator), defaultEncodeOptions, Quoting (Always, AsNeeded, Never))
import Data.Sv.Encode.Type (Encode (Encode, getEncode), NameEncode (NameEncode, unNamedE))
import Data.Sv.Structure.Newline (newlineToBuilder)

-- | Make an 'Encode' from a function that builds one 'Field'.
mkEncodeBS :: (a -> LBS.ByteString) -> Encode a
mkEncodeBS = unsafeBuilder . fmap BS.lazyByteString

-- | Make an 'Encode' from a function that builds one 'Field'.
mkEncodeWithOpts :: (EncodeOptions -> a -> BS.Builder) -> Encode a
mkEncodeWithOpts = Encode . fmap (fmap pure)

-- | Make an encode from any function that returns a ByteString 'Builder'.
unsafeBuilder :: (a -> BS.Builder) -> Encode a
unsafeBuilder b = Encode (\_ a -> pure (b a))
{-# INLINE unsafeBuilder #-}

-- | Encode the given list using the given 'Encode', configured by the given
-- 'EncodeOptions'.
encode :: Encode a -> EncodeOptions -> [a] -> LBS.ByteString
encode enc opts = BS.toLazyByteString . encodeBuilder enc opts

-- | Encode the given list with a header using the given 'NameEncode',
-- configured by the given 'EncodeOptions'.
encodeNamed :: NameEncode a -> EncodeOptions -> [a] -> LBS.ByteString
encodeNamed enc opts = BS.toLazyByteString . encodeNamedBuilder enc opts

-- | Encode, writing the output to a file handle.
encodeToHandle :: Encode a -> EncodeOptions -> [a] -> Handle -> IO ()
encodeToHandle enc opts as h =
  BS.hPutBuilder h (encodeBuilder enc opts as)

-- | Encode with a header, writing the output to a file handle.
encodeNamedToHandle :: NameEncode a -> EncodeOptions -> [a] -> Handle -> IO ()
encodeNamedToHandle enc opts as h =
  BS.hPutBuilder h (encodeNamedBuilder enc opts as)

-- | Encode, writing to a file. This way is more efficient than encoding to
-- a 'ByteString' and then writing to file.
encodeToFile :: Encode a -> EncodeOptions -> [a] -> FilePath -> IO ()
encodeToFile = genericEncodeToFile encodeToHandle

-- | Encode with a header, writing to a file. This way is more efficient
-- than encoding to a 'ByteString' and then writing to file.
encodeNamedToFile :: NameEncode a -> EncodeOptions -> [a] -> FilePath -> IO ()
encodeNamedToFile = genericEncodeToFile encodeNamedToHandle

genericEncodeToFile
  :: (enc -> EncodeOptions -> [a] -> Handle -> IO ())
  -> enc -> EncodeOptions -> [a] -> FilePath -> IO ()
genericEncodeToFile encHandle enc opts as fp = do
  h <- openFile fp WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  hSetBinaryMode h True
  encHandle enc opts as h
  hClose h
{-# INLINE genericEncodeToFile #-}

-- | Encode to a ByteString 'Builder', which is useful if you are going
-- to combine the output with other 'ByteString's.
encodeBuilder :: Encode a -> EncodeOptions -> [a] -> BS.Builder
encodeBuilder e opts as =
  let enc = encodeRowBuilder e opts
      nl  = newlineToBuilder (_newline opts)
      terminal = if _terminalNewline opts then nl else mempty
  in  case as of
    [] -> terminal
    (a:as') -> enc a <> mconcat [nl <> enc a' | a' <- as'] <> terminal

-- | Encode with column names to a ByteString 'Builder', which is useful
-- if you are going to combine the output with other 'ByteString's.
encodeNamedBuilder :: NameEncode a -> EncodeOptions -> [a] -> BS.Builder
encodeNamedBuilder ne opts as =
  case runNamed ne of
    (e, builders) ->
      let mkHeader = fold . addSeparators opts . addQuoting opts
          addQuoting = fmap . enquote
          nl = newlineToBuilder (_newline opts)
          header = mkHeader builders
      in  header <> case as of
        []    -> if _terminalNewline opts then nl else mempty
        (_:_) -> nl <> encodeBuilder e opts as

-- | Encode one row only
encodeRow :: Encode a -> EncodeOptions -> a -> LBS.ByteString
encodeRow e opts = BS.toLazyByteString . encodeRowBuilder e opts

-- | Encode one row only, as a ByteString 'Builder'
encodeRowBuilder :: Encode a -> EncodeOptions -> a -> BS.Builder
encodeRowBuilder e opts =
  fold . addSeparators opts . getEncode e opts

addSeparators :: HasSeparator s => s -> Seq BS.Builder -> Seq BS.Builder
addSeparators opts = intersperseSeq (BS.word8 (view separator opts))
{-# INLINE addSeparators #-}

-- | Encode this 'Data.ByteString.ByteString' every time, ignoring the input.
const :: Strict.ByteString -> Encode a
const b = contramap (pure b) byteString

-- | Build an 'Encode' using a type's 'Show' instance.
show :: Show a => Encode a
show = contramap P.show string

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
char = escaped BS.charUtf8

quotingIsNecessary :: EncodeOptions -> LBS.ByteString -> Bool
quotingIsNecessary opts =
  LBS.any p
    where
      sep = _encodeSeparator opts
      p :: Word8 -> Bool
      p w =
        w == sep ||
        w == 10  || -- lf
        w == 13  || -- cr
        w == 34     -- double quote

enquote :: EncodeOptions -> BS.Builder -> BS.Builder
enquote opts s =
  let lbs = BS.toLazyByteString s
      quoted = quote lbs
  in  case _quoting opts of
        Never ->
          s
        AsNeeded ->
          if quotingIsNecessary opts lbs
          then quoted
          else s
        Always -> quoted

quote :: LBS.ByteString -> BS.Builder
quote bs =
  let q = BS.charUtf8 '"'
      bs' = BS.lazyByteString (escapeQuotes bs)
  in  q <> bs' <> q

escapeQuotes :: LBS.ByteString -> LBS.ByteString
escapeQuotes = LBS.concatMap duplicateQuote
  where
    duplicateQuote :: Word8 -> LBS.ByteString
    duplicateQuote 34 = LBS.pack [34,34] -- 34 = quote
    duplicateQuote c  = LBS.singleton c

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
--
-- This version satisfies the roundtrip property. If that doesn't matter to you,
-- use the faster version 'doubleFast'
double :: Encode Double
double = unsafeBuilder BS.doubleDec

-- | Encode a 'Double' really quickly. This version uses the @double-conversion@
-- package.
doubleFast :: Encode Double
doubleFast = contramap DC.toShortest unsafeByteString

-- | Encode a 'String'
string :: Encode String
string = escaped BS.stringUtf8

-- | Encode a 'Data.Text.Text'
text :: Encode T.Text
text = escaped (BS.byteString . T.encodeUtf8)

-- | Encode a strict 'Data.ByteString.ByteString'
byteString :: Encode Strict.ByteString
byteString = escaped BS.byteString

-- | Encode a lazy 'Data.ByteString.Lazy.ByteString'
lazyByteString :: Encode LBS.ByteString
lazyByteString = escaped BS.lazyByteString

escaped :: (s -> BS.Builder) -> Encode s
escaped build =
  mkEncodeWithOpts $ \opts s ->
    enquote opts (build s)

-- | Encode a 'Bool' as True or False
boolTrueFalse :: Encode Bool
boolTrueFalse = mkEncodeBS $ B.bool "False" "True"

-- | Encode a 'Bool' as true of false
booltruefalse :: Encode Bool
booltruefalse = mkEncodeBS $ B.bool "false" "true"

-- | Encode a 'Bool' as yes or no
boolyesno :: Encode Bool
boolyesno = mkEncodeBS $ B.bool "no" "yes"

-- | Encode a 'Bool' as Yes or No
boolYesNo :: Encode Bool
boolYesNo = mkEncodeBS $ B.bool "No" "Yes"

-- | Encode a 'Bool' as Y or N
boolYN :: Encode Bool
boolYN = mkEncodeBS $ B.bool "N" "Y"

-- | Encode a 'Bool' as 1 or 0
bool10 :: Encode Bool
bool10 = mkEncodeBS $ B.bool "0" "1"

mkNamed :: Encode a -> Seq BS.Builder -> NameEncode a
mkNamed enc b = NameEncode (ComposeFC (writer (enc, b)))

-- | Attach a column name to an 'Encode'. This is used for building 'Encode's
-- with headers.
--
-- Best used with @OverloadedStrings@
named :: BS.Builder -> Encode a -> NameEncode a
named name enc = mkNamed enc (pure name)

-- | Synonym for 'named'.
--
-- Mnemonic: __D__ot colon names __D__ecoders, __E__qual colon names __E__ncoders.
(=:) :: BS.Builder -> Encode a -> NameEncode a
(=:) = named

runNamed :: NameEncode a -> (Encode a, Seq BS.Builder)
runNamed = runWriter . getComposeFC . unNamedE

unNameEncode :: NameEncode a -> Encode a
unNameEncode = view _1 . runWriter . getComposeFC . unNamedE

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
encodeOfMay = contramap . preview

-- | Encode a 'String' really quickly.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeString :: Encode String
unsafeString = unsafeBuilder BS.stringUtf8

-- | Encode 'Data.Text.Text' really quickly.
-- If the text has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeText :: Encode T.Text
unsafeText = unsafeBuilder (BS.byteString . T.encodeUtf8)

-- | Encode ByteString 'Data.ByteString.Builder.Builder' really quickly.
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

-- | Encode this 'Data.ByteString.ByteString' really quickly every time, ignoring the input.
-- If the string has quotes in it, they will not be escaped properly, so
-- the result maybe not be valid CSV
unsafeConst :: Strict.ByteString -> Encode a
unsafeConst b = contramap (pure b) unsafeByteString
