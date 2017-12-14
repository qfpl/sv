{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | This module contains data structures, combinators, and primitives for
-- decoding an 'Sv' into a list of your Haskell datatype.
--
-- A file can be read with 'decodeFromFile'. If you already have the text
-- data in memory, it can be decoded with 'parseDecode'.
-- You will need a 'FieldDecode' for your desired type.
--
-- A 'FieldDecode' can be built using the primitives in this file. 'FieldDecode'
-- is an 'Applicative' and an 'Alternative', allowing for composition of these
-- values.

module Data.Sv.Decode (
  decode
, parseDecode
, decodeFromFile
, contents
, byteString
, utf8
, lazyByteString
, string
, lazyText
, trimmed
, ignore
, replace
, unit
, int
, integer
, float
, double
, choice
, element
, choiceE
, orElse
, orElseE
, categorical
, decodeRead
, decodeRead'
, decodeReadWithMsg
, parser
, trifecta
, attoparsec
, parsec
, module Data.Sv.Decode.Error
, module Data.Sv.Decode.Field
, module Data.Sv.Decode.State
, module Data.Sv.Decode.Type
, FieldContents
) where

import Control.Lens (alaf)
import Control.Monad.IO.Class (MonadIO)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A (Parser)
import Data.Bifunctor (bimap, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Functor.Alt (Alt ((<!>)))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First))
import Data.Readable (Readable (fromBS))
import Data.Semigroup.Foldable (asum1)
import Data.Set (Set, fromList, member)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT
import Text.Parsec (Parsec)
import qualified Text.Parsec as P (parse)
import Text.Trifecta (CharParsing, eof, parseByteString, parseFromFileEx)
import qualified Text.Trifecta as T (Parser)

import Data.Sv.Sv (Separator, Sv, Headedness, recordList)
import Data.Sv.Decode.Error
import Data.Sv.Decode.Field
import Data.Sv.Decode.State
import Data.Sv.Decode.Type
import Data.Sv.Field (FieldContents)
import Data.Sv.Parser (separatedValues)
import Text.Babel (Textual, retext, showT, toByteString, toLazyByteString, toString, toText, trim)

-- | Decodes a sv into a list of its values using the provided 'FieldDecode'
decode :: (Textual s, Textual e) => FieldDecode e s a -> Sv s -> DecodeValidation e [a]
decode f = traverse (promote f) . recordList

-- | Parse text as an Sv, and then decode it with the given decoder.
parseDecode ::
  (Textual s, Textual e)
  => FieldDecode e s a
  -> Separator
  -> Headedness
  -> s
  -> DecodeValidation e [a]
parseDecode d sep h s =
  let parseResult = resultToDecodeError BadParse . parseByteString (separatedValues sep h) mempty . toByteString $ s
  in  parseResult `bindValidation` decode d

decodeFromFile ::
  (MonadIO m, Textual e, Textual s)
  => FieldDecode e s a
  -> Separator
  -> Headedness
  -> FilePath
  -> m (DecodeValidation e [a])
decodeFromFile d sep h fp =
  let decodeResult r = resultToDecodeError BadParse r `bindValidation` decode d
  in  decodeResult <$> parseFromFileEx (separatedValues sep h) fp

-- | Get the contents of a field without doing any decoding. This never fails.
contents :: FieldContents s => FieldDecode e s s
contents = fieldDecode pure

-- | Get the contents of a field as a bytestring.
byteString :: FieldContents s => FieldDecode e s ByteString
byteString = toByteString <$> contents

-- | Get the contents of a field as a bytestring
utf8 :: IsString e => FieldDecode e ByteString Text
utf8 = contents >>==
  either (badDecode . fromString . show) pure . decodeUtf8'

lazyByteString :: FieldContents s => FieldDecode e s LBS.ByteString
lazyByteString = toLazyByteString <$> contents

string :: FieldContents s => FieldDecode e s String
string = toString <$> contents

lazyText :: IsString e => FieldDecode e ByteString LT.Text
lazyText = LT.fromStrict <$> text

-- | Decode the field as 'Text'. If your input string is a ByteString,
-- consider using 'utf8'` instead.
text :: FieldContents s => FieldDecode e s Text
text = toText <$> contents

trimmed :: FieldContents s => FieldDecode e s s
trimmed = trim <$> contents

ignore :: FieldContents s => FieldDecode e s ()
ignore = replace ()

replace :: FieldContents s => a -> FieldDecode e s a
replace a = a <$ contents

unit :: FieldContents s => FieldDecode e s ()
unit = ignore

int :: (FieldContents s, Textual e) => FieldDecode e s Int
int = named "int"

integer :: (FieldContents s, Textual e) => FieldDecode e s Integer
integer = named "integer"

float :: (FieldContents s, Textual e) => FieldDecode e s Float
float = named "float"

double :: (FieldContents s, Textual e) => FieldDecode e s Double
double = named "double"

choice :: FieldDecode e s a -> FieldDecode e s a -> FieldDecode e s a
choice = (<!>)

element :: NonEmpty (FieldDecode e s a) -> FieldDecode e s a
element = asum1

choiceE :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
choiceE a b = fmap Left a <!> fmap Right b

orElse :: FieldContents s => FieldDecode e s a -> a -> FieldDecode e s a
orElse f a = f <!> replace a

orElseE :: FieldContents s => FieldDecode e s b -> a -> FieldDecode e s (Either a b)
orElseE b a = swapE <$> choiceE b (replace a)
  where
    swapE = either Right Left

categorical :: forall a s e. (FieldContents s, Ord s, Textual e, Show a) => [(a, [s])] -> FieldDecode e s a
categorical as =
  let as' :: [(a, Set s)]
      as' = fmap (second fromList) as
      go :: s -> (a, Set s) -> Maybe a
      go s (a, set) =
        if s `member` set
        then Just a
        else Nothing
  in  contents >>== \s ->
  decodeMay' (UnknownCanonicalValue (retext s) (fmap (bimap showT (fmap retext)) as)) $
    alaf First foldMap (go s) as'

decodeRead :: (Readable a, FieldContents s, Textual e) => FieldDecode e s a
decodeRead = decodeReadWithMsg (mappend "Couldn't parse " . retext)

decodeRead' :: (Textual e, Readable a) => e -> FieldDecode e ByteString a
decodeRead' e = decodeReadWithMsg (const e)

decodeReadWithMsg :: (FieldContents s, Textual e, Readable a) => (s -> e) -> FieldDecode e s a
decodeReadWithMsg e = trimmed >>== \c ->
  maybe (badDecode (e c)) pure . fromBS . toByteString $ c

named :: (Readable a, FieldContents s, Textual e) => s -> FieldDecode e s a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap n . listToMaybe
      n'' = fromString (n' (toString name))
      space = " "
  in  decodeReadWithMsg $ \bs ->
        mconcat ["Couldn't parse \"", retext bs, "\" as a", n'', space, retext name]

---- Promoting parsers to 'FieldDecode's

-- | Build a 'FieldDecode' from a parser from the 'parsers' library.
parser :: (FieldContents s, Textual e) => (forall m . CharParsing m => m a) -> FieldDecode e s a
parser = trifecta

-- | Build a 'FieldDecode' from a Trifecta parser
trifecta :: (FieldContents s, Textual e) => T.Parser a -> FieldDecode e s a
trifecta =
  mkParserFunction
    (resultToDecodeError BadDecode)
    (flip parseByteString mempty)

-- | Build a 'FieldDecode' from an Attoparsec parser
attoparsec :: (FieldContents s, Textual e) => A.Parser a -> FieldDecode e s a
attoparsec =
  mkParserFunction
    (eitherToDecodeError (BadDecode . fromString))
    parseOnly

-- | Build a 'FieldDecode' from a Parsec parser
parsec :: (FieldContents s, Textual e) => Parsec ByteString () a -> FieldDecode e s a
parsec =
  mkParserFunction
    (eitherToDecodeError (BadDecode . showT))
    (\p s -> P.parse p mempty s)

mkParserFunction ::
  (CharParsing p, FieldContents s, Textual e)
  => (f a -> DecodeValidation e a)
  -> (p a -> ByteString -> f a)
  -> p a
  -> FieldDecode e s a
mkParserFunction err run p =
  let p' = p <* eof
  in  byteString >>== (err . run p')
