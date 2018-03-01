{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Sv.Decode
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module contains data structures, combinators, and primitives for
decoding an 'Sv' into a list of your Haskell datatype.

A file can be read with 'parseDecodeFromFile'. If you already have the text
data in memory, it can be decoded with 'parseDecode'.
You will need a 'FieldDecode' for your desired type.

A 'FieldDecode' can be built using the primitives in this file. 'FieldDecode'
is an 'Applicative' and an 'Alternative', allowing for composition of these
values.

This module is intended to be imported qualified like so

@import qualified Data.Sv.Decode as D@
-}

module Data.Sv.Decode (
  -- * Running FieldDecodes
  decode
, parseDecode
, parseDecode'
, parseDecodeFromFile
, parseDecodeFromFile'

-- * Convenience constructors and functions
, decodeMay
, decodeEither
, decodeEither'
, mapErrors
, alterInput
, alterInputIso

-- * Primitives
-- ** Field-based
, contents
, untrimmed
, raw
, char
, byteString
, utf8
, lazyUtf8
, ascii
, lazyByteString
, string
, int
, integer
, float
, double
, boolean
, ignore
, replace
, exactly
, emptyField
-- ** Row-based
, row
, rowWithSpacing

-- * Combinators
, Alt ((<!>))
, Applicative (pure, (<*>))
, choice
, element
, optionalField
, ignoreFailure
, orEmpty
, either
, orElse
, orElseE
, categorical
, categorical'

-- * Building FieldDecodes from Readable
, decodeRead
, decodeRead'
, decodeReadWithMsg

-- * Building FieldDecodes from parsers
, withTrifecta
, withAttoparsec
, withParsec

-- TODO
, module Data.Sv.Decode.Error
, module Data.Sv.Decode.Field
, module Data.Sv.Decode.Type
) where

import Prelude hiding (either)
import qualified Prelude as P

import Control.Lens (AnIso, alaf, review, view, withIso)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.State (state)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A (Parser)
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (First (First))
import Data.Profunctor (lmap)
import Data.Readable (Readable (fromBS))
import Data.Semigroup (Semigroup ((<>)), sconcat)
import Data.Semigroup.Foldable (asum1)
import Data.Set (Set, fromList, member)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec (Parsec)
import qualified Text.Parsec as P (parse)
import qualified Text.Trifecta as Tri

import Data.Sv.Decode.Error
import Data.Sv.Decode.Field
import Data.Sv.Decode.Type
import Data.Sv.Parse (SvParser, parseSv', parseSvFromFile')
import qualified Data.Sv.Parse as P (trifecta)
import Data.Sv.Parse.Options (ParseOptions)
import Data.Sv.Syntax.Field (Field (Unquoted, Quoted), fieldContents, SpacedField, Spaced (Spaced))
import Data.Sv.Syntax.Sv (Sv, recordList)
import Text.Space (AsHorizontalSpace (_HorizontalSpace), Spaces, spacedValue)

-- | Decodes a sv into a list of its values using the provided 'FieldDecode'
decode :: FieldDecode' s a -> Sv s -> DecodeValidation s [a]
decode f = traverse (promote f) . recordList

-- | Parse a 'ByteString' as an Sv, and then decode it with the given decoder.
--
-- This version uses 'Trifecta' to parse the 'ByteString', which is assumed to
-- be UTF-8 encoded. If you want a different library, use 'parseDecode''.
parseDecode ::
  FieldDecode' ByteString a
  -> ParseOptions ByteString
  -> ByteString
  -> DecodeValidation ByteString [a]
parseDecode = parseDecode' P.trifecta

-- | Parse text as an Sv, and then decode it with the given decoder.
--
-- This version lets you choose which parsing library to use by providing an
-- 'SvParser'. Common selections are 'trifecta' and 'attoparsecByteString'.
parseDecode' ::
  SvParser s
  -> FieldDecode' s a
  -> ParseOptions s
  -> s
  -> DecodeValidation s [a]
parseDecode' svp d opts s =
  P.either badDecode pure (parseSv' svp opts s) `bindValidation` decode d

-- | Load a file, parse it, and decode it.
--
-- This version uses Trifecta to parse the file, which is assumed to be UTF-8
-- encoded.
parseDecodeFromFile ::
  MonadIO m
  => FieldDecode' ByteString a
  -> ParseOptions ByteString
  -> FilePath
  -> m (DecodeValidation ByteString [a])
parseDecodeFromFile = parseDecodeFromFile' P.trifecta

-- | Load a file, parse it, and decode it.
--
-- This version lets you choose which parsing library to use by providing an
-- 'SvParser'. Common selections are 'trifecta' and 'attoparsecByteString'.
parseDecodeFromFile' ::
  MonadIO m
  => SvParser s
  -> FieldDecode' s a
  -> ParseOptions s
  -> FilePath
  -> m (DecodeValidation s [a])
parseDecodeFromFile' svp d opts fp = do
  sv <- parseSvFromFile' svp opts fp
  pure (P.either badDecode pure sv `bindValidation` decode d)

-- | Build a 'Decode', given a function that returns 'Maybe'.
--
-- Return the given error if the function returns 'Nothing'.
decodeMay :: DecodeError e -> (s -> Maybe a) -> FieldDecode e s a
decodeMay e f = fieldDecode (validateMay e . f)

-- | Build a 'Decode', given a function that returns 'Either'.
decodeEither :: (s -> Either (DecodeError e) a) -> FieldDecode e s a
decodeEither f = fieldDecode (validateEither . f)

-- | Build a 'Decode', given a function that returns 'Either', and a function to
-- build the error.
decodeEither' :: (e -> DecodeError e') -> (s -> Either e a) -> FieldDecode e' s a
decodeEither' e f = fieldDecode (validateEither' e . f)

-- | Succeeds with the whole field structure, including spacing and quoting information
raw :: FieldDecode e s (SpacedField s)
raw = fieldDecodeWithSpaces pure

-- | Returns the field contents. This keeps the spacing around an unquoted field.
untrimmed :: (AsHorizontalSpace s, Monoid s) => FieldDecode e s s
untrimmed =
  let sp :: (Monoid b, AsHorizontalSpace b) => Spaces -> b
      sp = foldMap (review _HorizontalSpace)
      spaceIfNecessary (Spaced b a f) = case f of
        Unquoted s -> mconcat [sp b, s, sp a]
        Quoted _ _ -> view fieldContents f
  in  fmap spaceIfNecessary raw

-- | Get the contents of a field without doing any decoding. This never fails.
contents :: FieldDecode e s s
contents = fieldDecode pure

-- | Grab the whole row as a raw 'Vector'
row :: FieldDecode e s (Vector s)
row = (fmap . fmap) (view (spacedValue.fieldContents)) rowWithSpacing

-- | Grab the whole row, including all spacing and quoting information,
-- as a raw 'Vector'
rowWithSpacing :: FieldDecode e s (Vector (SpacedField s))
rowWithSpacing =
  FieldDecode . Compose . DecodeState . ReaderT $ \v ->
    state (const (pure v, Ind (V.length v)))

-- | Get a field that's a single char. This will fail if there are mulitple
-- characters in the field.
char :: FieldDecode' ByteString Char
char = string >>== \cs -> case cs of
  [] -> badDecode "Expected single char but got empty string"
  (c:[]) -> pure c
  (_:_:_) -> badDecode ("Expected single char but got " <> fromString cs)

-- | Get the contents of a field as a bytestring.
--
-- Alias for 'contents'
byteString :: FieldDecode' ByteString ByteString
byteString = contents

-- | Get the contents of a UTF8 encoded field as 'Text'
utf8 :: FieldDecode' ByteString Text
utf8 = contents >>==
  P.either (badDecode . UTF8.fromString . show) pure . decodeUtf8'

-- | Get the contents of a field as a 'Data.Text.Lazy.Text'
lazyUtf8 :: FieldDecode' ByteString LT.Text
lazyUtf8 = LT.fromStrict <$> utf8

-- | Get the contents of an ASCII encoded field as 'Text'
ascii ::  FieldDecode' ByteString Text
ascii = utf8

-- | Get the contents of a field as a 'Data.ByteString.Lazy.ByteString'
lazyByteString :: FieldDecode' ByteString LBS.ByteString
lazyByteString = LBS.fromStrict <$> contents

-- | Get the contents of a field as a 'String'
string :: FieldDecode' ByteString String
string = UTF8.toString <$> contents

-- | Throw away the contents of a field. This is useful for skipping unneeded fields.
ignore :: FieldDecode e s ()
ignore = replace ()

-- | Throw away the contents of a field, and return the given value.
replace :: a -> FieldDecode e s a
replace a = a <$ contents

-- | Exactly this string, or else fail
exactly :: (Semigroup s, Eq s, IsString s) => s -> FieldDecode' s s
exactly s = contents >>== \z ->
  if s == z
  then pure s
  else badDecode (sconcat ("'":|[z,"' was not equal to '",s,"'"]))

-- | Decode a field as an 'Int'
int :: FieldDecode' ByteString Int
int = named "int"

-- | Decode a field as an 'Integer'
integer :: FieldDecode' ByteString Integer
integer = named "integer"

-- | Decode a field as a 'Float'
float :: FieldDecode' ByteString Float
float = named "float"

-- | Decode a field as a 'Double'
double :: FieldDecode' ByteString Double
double = named "double"

-- | Decode a field as a 'Boolean'
--
-- This is quite tolerant to different forms a boolean might take.
boolean :: (IsString s, Ord s) => FieldDecode' s Bool
boolean = boolean' fromString

boolean' :: Ord s => (String -> s) -> FieldDecode' s Bool
boolean' s =
  categorical' [
    (False, fmap s ["false", "False", "FALSE", "f", "F", "0", "n", "N", "no", "No", "NO", "off", "Off", "OFF"])
  , (True, fmap s ["true", "True", "TRUE", "t", "T", "1", "y", "Y", "yes", "Yes", "YES", "on", "On", "ON"])
  ]

-- | Succeed only when the given field is the empty string.
--
-- The empty string surrounded in quotes is still the empty string.
emptyField :: (Eq s, IsString s, Semigroup s) => FieldDecode' s ()
emptyField = contents >>== \c ->
  if c == fromString "" then
    pure ()
  else
    badDecode ("Expected emptiness but got: " <> c)

-- | Choose the leftmost FieldDecode that succeeds. Alias for '<!>'
choice :: FieldDecode e s a -> FieldDecode e s a -> FieldDecode e s a
choice = (<!>)

-- | Choose the leftmost FieldDecode that succeeds. Alias for 'asum1'
element :: NonEmpty (FieldDecode e s a) -> FieldDecode e s a
element = asum1

-- | Try the given 'FieldDecode'. If it fails, instead succeed with 'Nothing'.
ignoreFailure :: FieldDecode e s a -> FieldDecode e s (Maybe a)
ignoreFailure a = Just <$> a <!> Nothing <$ ignore

-- | Try the given 'FieldDecode'. If the field is the empty string, succeed with 'Nothing'.
orEmpty :: (Eq s, IsString s, Semigroup s) => FieldDecode' s a -> FieldDecode' s (Maybe a)
orEmpty a = Nothing <$ emptyField <!> Just <$> a

-- | Try the given 'FieldDecode'. If it fails, succeed without consuming anything.
optionalField :: FieldDecode e s a -> FieldDecode e s (Maybe a)
optionalField a = Just <$> a <!> pure Nothing

-- | Try the left, then try the right, and wrap the winner in an 'Either'.
--
-- This is left-biased, meaning if they both succeed, left wins.
either :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
either a b = fmap Left a <!> fmap Right b

-- | Try the given decoder, otherwise succeed with the given value.
orElse :: FieldDecode e s a -> a -> FieldDecode e s a
orElse f a = f <!> replace a

-- | Try the given decoder, or if it fails succeed with the given value, in an 'Either'.
orElseE :: FieldDecode e s b -> a -> FieldDecode e s (Either a b)
orElseE b a = fmap Right b <!> replace (Left a)

-- | Decode categorical data, given a list of the values and the strings which match them.
--
-- This is very useful for sum types with nullary constructors.
categorical :: (Ord s, Show a) => [(a, s)] -> FieldDecode' s a
categorical = categorical' . fmap (fmap pure)

-- | Decode categorical data, given a list of the values and the strings which match them.
--
-- This version allows for multiple strings to match each value.
-- For an example of its usage, see the source for 'boolean'.
categorical' :: forall s a . (Ord s, Show a) => [(a, [s])] -> FieldDecode' s a
categorical' as =
  let as' :: [(a, Set s)]
      as' = fmap (second fromList) as
      go :: s -> (a, Set s) -> Maybe a
      go s (a, set) =
        if s `member` set
        then Just a
        else Nothing
  in  contents >>== \s ->
    validateMay (UnknownCanonicalValue s (fmap snd as)) $
      alaf First foldMap (go s) as'

-- | Use the 'Readable' instance to try to decode the given value.
decodeRead :: Readable a => FieldDecode' ByteString a
decodeRead = decodeReadWithMsg (mappend "Couldn't parse ")

-- | Use the 'Readable' instance to try to decode the given value,
-- or fail with the given error message.
decodeRead' :: Readable a => ByteString -> FieldDecode' ByteString a
decodeRead' e = decodeReadWithMsg (const e)

-- | Use the 'Readable' instance to try to decode the given value,
-- or use the value to build an error message.
decodeReadWithMsg :: Readable a => (ByteString -> e) -> FieldDecode e ByteString a
decodeReadWithMsg e = contents >>== \c ->
  maybe (badDecode (e c)) pure . fromBS $ c

-- | Given the name of a type, try to decode it using 'Readable', 
named :: Readable a => ByteString -> FieldDecode' ByteString a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap (n . fst) . UTF8.uncons
      n'' = n' name
      space = " "
  in  decodeReadWithMsg $ \bs ->
        mconcat ["Couldn't parse \"", bs, "\" as a", n'', space, name]

-- | Map over the errors of a 'FieldDecode'
--
-- To map over the other two paramters, use the 'Profunctor' instance.
mapErrors :: (e -> x) -> FieldDecode e s a -> FieldDecode x s a
mapErrors f (FieldDecode (Compose r)) = FieldDecode (Compose (fmap (first (fmap f)) r))

-- | This transforms a @FieldDecode' s a@ into a @FieldDecode' t a@. It needs
-- functions in both directions because the errors can include fragments of the
-- input.
--
-- @alterInput :: (s -> t) -> (t -> s) -> FieldDecode' s a -> FieldDecode' t a@
alterInput :: (e -> x) -> (t -> s) -> FieldDecode e s a -> FieldDecode x t a
alterInput f g = mapErrors f . lmap g

-- | Like @alterInput@, but uses an @Control.Lens.Iso@
alterInputIso :: AnIso e s x t -> FieldDecode e s a -> FieldDecode x t a
alterInputIso i = withIso i alterInput

---- Promoting parsers to 'FieldDecode's

-- | Build a 'FieldDecode' from a Trifecta parser
withTrifecta :: Tri.Parser a -> FieldDecode' ByteString a
withTrifecta =
  mkParserFunction
    (validateTrifectaResult (BadDecode . UTF8.fromString))
    (flip Tri.parseByteString mempty)

-- | Build a 'FieldDecode' from an Attoparsec parser
withAttoparsec :: A.Parser a -> FieldDecode' ByteString a
withAttoparsec =
  mkParserFunction
    (validateEither' (BadDecode . fromString))
    parseOnly

-- | Build a 'FieldDecode' from a Parsec parser
withParsec :: Parsec ByteString () a -> FieldDecode' ByteString a
withParsec =
  -- Parsec will include a position, but it will only confuse the user
  -- since it won't correspond obviously to a position in their source file.
  let dropPos = drop 1 . dropWhile (/= ':')
  in  mkParserFunction
    (validateEither' (BadDecode . UTF8.fromString . dropPos . show))
    (\p s -> P.parse p mempty s)

mkParserFunction ::
  Tri.CharParsing p
  => (f a -> DecodeValidation ByteString a)
  -> (p a -> ByteString -> f a)
  -> p a
  -> FieldDecode' ByteString a
mkParserFunction err run p =
  let p' = p <* Tri.eof
  in  byteString >>== (err . run p')
{-# INLINE mkParserFunction #-}
