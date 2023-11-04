{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Sv.Decode.Core
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module contains data structures, combinators, and primitives for
decoding a CSV into a list of your Haskell datatype.

A 'Decode' can be built using the primitives in this file. 'Decode'
is an 'Applicative' and an 'Data.Functor.Alt.Alt', allowing for composition
of these values with '<*>' and '<!>'

The primitive 'Decode's in this file which use 'ByteString' expect UTF-8
encoding. The Decode type has an instance of 'Data.Profunctor.Profunctor',
so you can 'lmap' or 'alterInput' to reencode on the way in.

This module is intended to be imported qualified like so

@
import qualified Data.Sv.Decode.Core as D
@
-}

module Data.Sv.Decode.Core (
  -- * The types
  Decode (..)
, Decode'
, DecodeValidation
, DecodeError (..)
, DecodeErrors (..)

-- * Running Decodes
, decode

-- * Convenience constructors and functions
, decodeMay
, decodeEither
, decodeEither'
, mapErrors
, alterInput

-- * Primitive Decodes
-- ** Name-based
, column
, (.:)
-- ** Field-based
, contents
, char
, byteString
, utf8
, lazyUtf8
, lazyByteString
, string
, int
, integer
, float
, double
, rational
, boolean
, boolean'
, ignore
, replace
, exactly
, emptyField
-- ** Row-based
, row

-- * Combinators
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
, (>>==)
, (==<<)
, bindDecode

-- * Building Decodes from Read
, read
, read'

-- * Building Decodes from Readable
, decodeRead
, decodeRead'
, decodeReadWithMsg

-- * Building Decodes from parsers
, withTrifecta
, withAttoparsec
, withParsec
, withTextReader

-- * Working with errors
, onError
, decodeError
, unexpectedEndOfRow
, expectedEndOfRow
, unknownCategoricalValue
, badParse
, badDecode
, validateEither
, validateEitherWith
, validateMaybe

-- * Implementation details
, runDecode
, buildDecode
, mkDecode
, promote
, promote'
, runNamed
, anonymous
, makePositional
) where

import Prelude hiding (either, read)
import qualified Prelude

import Control.Lens (alaf)
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Control.Monad.State (state)
import Control.Monad.Writer (runWriter)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid (First (First), Last)
import Data.Profunctor (lmap)
import Data.Readable (Readable (fromBS))
import Data.Semigroup (Semigroup ((<>)), sconcat)
import Data.Semigroup.Foldable (asum1)
import Data.Semigroupoid (Semigroupoid (o))
import Data.Set (Set, fromList, member)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Text.Read as TR (Reader, rational)
import qualified Data.Text.Lazy as LT
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import GHC.Float (double2Float)
import Text.Parsec (Parsec)
import qualified Text.Parsec as P (parse)
import Text.Read (readMaybe)
import qualified Text.Trifecta as Tri

import Data.Sv.Decode.Error
import Data.Sv.Decode.Type

-- | Decodes a sv into a list of its values using the provided 'Decode'
decode :: Traversable f => Decode' ByteString a -> f (Vector ByteString) -> DecodeValidation ByteString (f a)
decode d = traverse (promote d)

-- | Build a 'Decode', given a function that returns 'Maybe'.
--
-- Return the given error if the function returns 'Nothing'.
decodeMay :: DecodeError e -> (s -> Maybe a) -> Decode e s a
decodeMay e f = mkDecode (validateMaybe e . f)

-- | Build a 'Decode', given a function that returns 'Either'.
decodeEither :: (s -> Either (DecodeError e) a) -> Decode e s a
decodeEither f = mkDecode (validateEither . f)

-- | Build a 'Decode', given a function that returns 'Either', and a function to
-- build the error.
decodeEither' :: (e -> DecodeError e') -> (s -> Either e a) -> Decode e' s a
decodeEither' e f = mkDecode (validateEitherWith e . f)

-- | Get the contents of a field without doing any decoding. This never fails.
contents :: Decode e s s
contents = mkDecode pure

-- | Grab the whole row as a 'Vector'
row :: Decode e s (Vector s)
row =
  Decode . Compose . DecodeState . ReaderT $ \v ->
    state (const (pure v, Ind (V.length v)))

-- | Get a field that's a single char. This will fail if there are mulitple
-- characters in the field.
char :: Decode' ByteString Char
char = string >>== \cs -> case cs of
  [] -> badDecode "Expected single char but got empty string"
  [c] -> pure c
  (_:_:_) -> badDecode ("Expected single char but got " <> UTF8.fromString cs)

-- | Get the contents of a field as a bytestring.
--
-- Alias for 'contents'
byteString :: Decode' ByteString ByteString
byteString = contents

-- | Get the contents of a UTF-8 encoded field as 'Text'
--
-- This will also work for ASCII text, as ASCII is a subset of UTF-8
utf8 :: Decode' ByteString Text
utf8 = contents >>==
  Prelude.either (badDecode . UTF8.fromString . show) pure . decodeUtf8'

-- | Get the contents of a field as a lazy 'Data.Text.Lazy.Text'
lazyUtf8 :: Decode' ByteString LT.Text
lazyUtf8 = LT.fromStrict <$> utf8

-- | Get the contents of a field as a lazy 'Data.ByteString.Lazy.ByteString'
lazyByteString :: Decode' ByteString LBS.ByteString
lazyByteString = LBS.fromStrict <$> contents

-- | Get the contents of a field as a 'String'
string :: Decode' ByteString String
string = UTF8.toString <$> contents

-- | Throw away the contents of a field. This is useful for skipping unneeded fields.
ignore :: Decode e s ()
ignore = replace ()

-- | Throw away the contents of a field, and return the given value.
replace :: a -> Decode e s a
replace a = a <$ contents

-- | Decode exactly the given string, or else fail.
exactly :: (Semigroup s, Eq s, IsString s) => s -> Decode' s s
exactly s = contents >>== \z ->
  if s == z
  then pure s
  else badDecode (sconcat ("'":|[z,"' was not equal to '",s,"'"]))

-- | Decode a UTF-8 'ByteString' field as an 'Int'
int :: Decode' ByteString Int
int = named "int"

-- | Decode a UTF-8 'ByteString' field as an 'Integer'
integer :: Decode' ByteString Integer
integer = named "integer"

-- | Decode a UTF-8 'ByteString' field as a 'Float'
float :: Decode' ByteString Float
float = double2Float <$> double

-- | Decode a UTF-8 'ByteString' field as a 'Double'
--
-- This is currently the fastest and most precise way to decode doubles.
double :: Decode' ByteString Double
double = withAttoparsec AC8.double <!> (
    contents >>== \s -> badDecode $ "Couldn't decode \"" <> s <> "\" as a double"
  )

{-# DEPRECATED rational "use double or float instead" #-}
-- | Decode a UTF-8 'ByteString' as any 'Floating' type (usually 'Double')
rational :: Floating a => Decode' ByteString a
rational = rat `o` utf8
  where
    rat = mapErrors encodeUtf8 (withTextReader TR.rational)

-- | Decode a field as a 'Bool'
--
-- This aims to be tolerant to different forms a boolean might take.
boolean :: (IsString s, Ord s) => Decode' s Bool
boolean = boolean' fromString

-- | Decode a field as a 'Bool'. This version lets you provide the fromString
-- function that's right for you, since 'Data.String.IsString' on a
-- 'Data.ByteString.ByteString' will do the wrong thing in the case of many
-- encodings such as UTF-16 or UTF-32.
--
-- This aims to be tolerant to different forms a boolean might take.
boolean' :: Ord s => (String -> s) -> Decode' s Bool
boolean' s =
  categorical' [
    (False, fmap s ["false", "False", "FALSE", "f", "F", "0", "n", "N", "no", "No", "NO", "off", "Off", "OFF"])
  , (True, fmap s ["true", "True", "TRUE", "t", "T", "1", "y", "Y", "yes", "Yes", "YES", "on", "On", "ON"])
  ]

-- | Succeed only when the given field is the empty string.
--
-- The empty string surrounded in quotes or spaces is still the empty string.
emptyField :: (Eq s, IsString s, Semigroup s) => Decode' s ()
emptyField = contents >>== \c ->
  unless (c == fromString "") (badDecode ("Expected emptiness but got: " <> c))

-- | Choose the leftmost 'Decode' that succeeds. Alias for '<!>'
choice :: Decode e s a -> Decode e s a -> Decode e s a
choice = (<!>)

-- | Choose the leftmost 'Decode' that succeeds. Alias for 'asum1'
element :: NonEmpty (Decode e s a) -> Decode e s a
element = asum1

-- | Try the given 'Decode'. If it fails, instead succeed with 'Nothing'.
ignoreFailure :: Decode e s a -> Decode e s (Maybe a)
ignoreFailure a = Just <$> a <!> Nothing <$ ignore

-- | If the field is the empty string, succeed with 'Nothing'.
-- Otherwise try the given 'Decode'.
orEmpty :: (Eq s, IsString s, Semigroup s) => Decode' s a -> Decode' s (Maybe a)
orEmpty a = Nothing <$ emptyField <!> Just <$> a

-- | Try the given 'Decode'. If it fails, succeed without consuming anything.
--
-- This usually isn't what you want. 'ignoreFailure' and 'orEmpty' are more
-- likely what you are after.
optionalField :: Decode e s a -> Decode e s (Maybe a)
optionalField a = Just <$> a <!> pure Nothing

-- | Try the first, then try the second, and wrap the winner in an 'Either'.
--
-- This is left-biased, meaning if they both succeed, left wins.
either :: Decode e s a -> Decode e s b -> Decode e s (Either a b)
either a b = fmap Left a <!> fmap Right b

-- | Try the given decoder, otherwise succeed with the given value.
orElse :: Decode e s a -> a -> Decode e s a
orElse f a = f <!> replace a

-- | Try the given decoder, or if it fails succeed with the given value, in an 'Either'.
orElseE :: Decode e s b -> a -> Decode e s (Either a b)
orElseE b a = fmap Right b <!> replace (Left a)

-- | Decode categorical data, given a list of the values and the strings which match them.
--
-- Usually this is used with sum types with nullary constructors.
--
-- > data TrafficLight = Red | Amber | Green
-- > categorical [(Red, "red"), (Amber, "amber"), (Green, "green")]
categorical :: (Ord s, Show a) => [(a, s)] -> Decode' s a
categorical = categorical' . fmap (fmap pure)

-- | Decode categorical data, given a list of the values and lists of strings
-- which match them.
--
-- This version allows for multiple strings to match each value, which is
-- useful for when the categories are inconsistently labelled.
--
-- > data TrafficLight = Red | Amber | Green
-- > categorical' [(Red, ["red", "R"]), (Amber, ["amber", "orange", "A"]), (Green, ["green", "G"])]
--
-- For another example of its usage, see the source for 'boolean'.
categorical' :: forall s a . (Ord s, Show a) => [(a, [s])] -> Decode' s a
categorical' as =
  let as' :: [(a, Set s)]
      as' = fmap (second fromList) as
      go :: s -> (a, Set s) -> Maybe a
      go s (a, set) =
        if s `member` set
        then Just a
        else Nothing
  in  contents >>== \s ->
    validateMaybe (UnknownCategoricalValue s (fmap snd as)) $
      alaf First foldMap (go s) as'

-- | Build a 'Decode' from a 'Read' instance
read :: Read a => Decode' ByteString a
read = read' (const $ badDecode "read decoder failed")

-- | Build a 'Decode' from a 'Read' instance.
--
-- This version takes a function which lets you build your own error message
-- in the event of a failure.
read' :: Read a => (ByteString -> DecodeValidation e a) -> Decode e ByteString a
read' mkError = contents >>== \c ->
  maybe (mkError c) pure $ readMaybe $ UTF8.toString c

-- | Use the 'Readable' instance to try to decode the given value.
decodeRead :: Readable a => Decode' ByteString a
decodeRead = decodeReadWithMsg (mappend "Couldn't decode ")

-- | Use the 'Readable' instance to try to decode the given value,
-- or fail with the given error message.
decodeRead' :: Readable a => ByteString -> Decode' ByteString a
decodeRead' e = decodeReadWithMsg (const e)

-- | Use the 'Readable' instance to try to decode the given value,
-- or use the value to build an error message.
decodeReadWithMsg :: Readable a => (ByteString -> e) -> Decode e ByteString a
decodeReadWithMsg e = contents >>== \c ->
  maybe (badDecode (e c)) pure . fromBS $ c

-- | Given the name of a type, try to decode it using 'Readable',
named :: Readable a => ByteString -> Decode' ByteString a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap (n . fst) . UTF8.uncons
      n'' = n' name
      space = " "
  in  decodeReadWithMsg $ \bs ->
        mconcat ["Couldn't decode \"", bs, "\" as a", n'', space, name]

-- | Map over the errors of a 'Decode'
--
-- To map over the other two parameters, use the 'Data.Profunctor.Profunctor' instance.
mapErrors :: (e -> x) -> Decode e s a -> Decode x s a
mapErrors f (Decode (Compose r)) =
  Decode (Compose (fmap (rnat (first (fmap f))) r))

-- | This transforms a @Decode' s a@ into a @Decode' t a@. It needs
-- functions in both directions because the errors can include fragments of the
-- input.
--
-- @alterInput :: (s -> t) -> (t -> s) -> Decode' s a -> Decode' t a@
alterInput :: (e -> x) -> (t -> s) -> Decode e s a -> Decode x t a
alterInput f g = mapErrors f . lmap g

---- Promoting parsers to 'Decode's

-- | Build a 'Decode' from a Trifecta parser
withTrifecta :: Tri.Parser a -> Decode' ByteString a
withTrifecta =
  mkParserFunction
    (validateTrifectaResult (BadDecode . UTF8.fromString))
    (`Tri.parseByteString` mempty)

-- | Build a 'Decode' from an Attoparsec parser
withAttoparsec :: A.Parser a -> Decode' ByteString a
withAttoparsec =
  mkParserFunction
    (validateEitherWith (BadDecode . fromString))
    A.parseOnly

-- | Build a 'Decode' from a Parsec parser
withParsec :: Parsec ByteString () a -> Decode' ByteString a
withParsec =
  -- Parsec will include a position, but it will only confuse the user
  -- since it won't correspond obviously to a position in their source file.
  let dropPos = drop 1 . dropWhile (/= ':')
  in  mkParserFunction
    (validateEitherWith (BadDecode . UTF8.fromString . dropPos . show))
    (`P.parse` mempty)

-- | Build a 'Decode' from a @Data.Text@ 'Data.Text.Read.Reader'
withTextReader :: TR.Reader a -> Decode' Text a
withTextReader ir =
  decodeEither $ \t -> case ir t of
    Left s ->
      let msg = "Couldn't decode \"" <> t <> "\": " <> T.pack s
      in  Left (BadDecode msg)
    Right (a,leftover) ->
      if T.null leftover
      then pure a
      else Left (BadDecode (
          "Leftover input during decoding: " <> leftover
        ))


mkParserFunction ::
  Tri.CharParsing p
  => (f a -> DecodeValidation ByteString a)
  -> (p a -> ByteString -> f a)
  -> p a
  -> Decode' ByteString a
mkParserFunction err run p =
  let p' = p <* Tri.eof
  in  byteString >>== (err . run p')
{-# INLINE mkParserFunction #-}

-- | This can be used to build a 'Decode' whose value depends on the
-- result of another 'Decode'. This is especially useful since 'Decode' is not
-- a 'Monad'.
--
-- If you need something like this but with more power, look at 'bindDecode'
(>>==) :: Decode e s a -> (a -> DecodeValidation e b) -> Decode e s b
(>>==) = flip (==<<)
infixl 1 >>==
{-# INLINE (>>==) #-}

-- | flipped '>>=='
(==<<) :: (a -> DecodeValidation e b) -> Decode e s a -> Decode e s b
(==<<) f d =
  buildDecode $ \vec i ->
    case runDecode d vec i of
      (v, l, i') -> (bindValidation v f, l, i')
infixr 1 ==<<

-- | Bind through a 'Decode'.
--
-- This bind does not agree with the 'Applicative' instance because it does
-- not accumulate multiple error values. This is a violation of the 'Monad'
-- laws, meaning 'Decode' is not a 'Monad'.
--
-- That is not to say that there is anything wrong with using this function.
-- It can be quite useful.
bindDecode :: Decode e s a -> (a -> Decode e s b) -> Decode e s b
bindDecode d f =
  buildDecode $ \v i ->
    case runDecode d v i of
      (Failure e, l, i') -> (Failure e, l, i')
      (Success a, l, i') ->
        case runDecode (f a) v i' of
          (v', l', i'') -> (v', l <> l', i'')

-- | Run a 'Decode', and based on its errors build a new 'Decode'.
onError :: Decode e s a -> (DecodeErrors e -> Decode e s a) -> Decode e s a
onError d f =
  buildDecode $ \v i ->
    case runDecode d v i of
      (Success a, l, i') -> (Success a, l, i')
      (Failure e, l, i') ->
        case runDecode (f e) v i' of
          (v',l',i'') -> (v',l <> l',i'')

-- | Build a 'Decode' from a function.
mkDecode :: (s -> DecodeValidation e a) -> Decode e s a
mkDecode f =
  Decode . Compose . DecodeState . ReaderT $ \v -> state $ \(Ind i) ->
    if i >= length v
    then (Compose (pure unexpectedEndOfRow), Ind i)
    else (Compose (pure (f (v ! i))), Ind (i+1))

-- | Promotes a 'Decode' to work on a whole 'Record' at once.
-- This does not need to be called by the user. Instead use 'decode'.
promote :: Decode' s a -> Vector s -> DecodeValidation s a
promote = promote' id
{-# INLINE promote #-}

-- | Promotes a 'Decode' to work on a whole 'Record' at once.
-- This does not need to be called by the user. Instead use 'decode'.
--
-- This version lets the error string and input string type pararms
-- differ, but needs a function to convert between them.
promote' :: (s -> e) -> Decode e s a -> Vector s -> DecodeValidation e a
promote' se dec vecField =
  let len = length vecField
  in  case runDecode dec vecField (Ind 0) of
    (d, l, Ind i) ->
      if i < len && and l
      then d *> expectedEndOfRow (V.force (fmap se (V.drop i vecField)))
      else d

-- | Convenience to get the underlying function out of a 'Decode' in a useful form
runDecode :: Decode e s a -> Vector s -> Ind -> (DecodeValidation e a, Last Bool, Ind)
runDecode = fmap (fmap z) . runDecodeState . getCompose . unwrapDecode
  where
    z (Compose wv, i) = case runWriter wv of
      (v,l) ->(v,l,i)
{-# INLINE runDecode #-}

-- | Convenience to get the underlying function out of a 'NameDecode' in a useful form
runNamed :: NameDecode e s a -> Map s Ind -> DecodeValidation e (Decode e s a)
runNamed = fmap getCompose . runReaderT . unNamed

-- | Promote a 'Decode' to a 'NameDecode' that doesn't look for any names
anonymous :: Decode e s a -> NameDecode e s a
anonymous = Named . ReaderT . pure . Compose . pure

-- | Given a header and a 'NameDecode', resolve header names to positions and
-- return a 'Decode'
makePositional :: Ord s => Vector s -> NameDecode e s a -> DecodeValidation e (Decode e s a)
makePositional names d =
  runNamed d . M.fromList $ zip (V.toList names) (Ind <$> [0..])

-- | This is the primitive for building decoders that work with columns
--
-- Look for the column with the given name and run the given decoder on it

column :: Ord s => s -> Decode' s a -> NameDecode' s a
column s d =
  Named . ReaderT $ \m -> case M.lookup s m of
    Nothing -> Compose (missingColumn s)
    Just i -> Compose . pure . buildDecode $ \vec _ ->
      case runDecode d vec i of
        (v, l, i') -> (v, l <> pure False, i')

-- | Infix alias for 'column'
--
-- Mnemonic: __D__ot colon names __D__ecoders, __E__qual colon names __E__ncoders.
(.:) :: Ord s => s -> Decode' s a -> NameDecode' s a
(.:) = column
{-# INLINE (.:) #-}
infixl 5 .:

rnat :: Functor f => (g a -> h a) -> Compose f g a -> Compose f h a
rnat gh (Compose fga) = Compose (fmap gh fga)
