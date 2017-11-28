{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Decode.ByteString where

import Control.Lens.Wrapped
import Data.Bifunctor (bimap, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First))
import Data.Semigroup (Semigroup ((<>)), sconcat)
import Data.Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (decodeUtf8')
import Data.Readable (Readable (fromBS))

import Data.Csv.Field (FieldContents)
import Data.Csv.Decode (FieldDecode, contents, decodeMay', (>>==))
import Data.Csv.Decode.Error
import Text.Babel

byteString :: FieldContents s => FieldDecode e s ByteString
byteString = toByteString <$> contents

bytestringUtf8 :: IsString e => FieldDecode e ByteString Text
bytestringUtf8 = contents >>==
  either (badDecode . fromString . show) pure . decodeUtf8'

lazyByteString :: FieldContents s => FieldDecode e s LBS.ByteString
lazyByteString = toLazyByteString <$> contents

string :: FieldContents s => FieldDecode e s String
string = toString <$> contents

lazyText :: IsString e => FieldDecode e ByteString LT.Text
lazyText = LT.fromStrict <$> text

text :: FieldContents s => FieldDecode e s Text
text = toText <$> contents

trimmed :: FieldContents s => FieldDecode e s s
trimmed = trim <$> contents

ignore :: FieldContents s => FieldDecode e s ()
ignore = () <$ contents

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

choice :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
choice a b = fmap Left a <!> fmap Right b

withDefault :: FieldContents s => FieldDecode e s b -> a -> FieldDecode e s (Either a b)
withDefault b a = swapE <$> choice b (replace a)
  where
    swapE = either Right Left

orElse :: FieldContents s => FieldDecode e s a -> a -> FieldDecode e s a
orElse f a = f <!> replace a

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
decodeRead = decodeReadWith ((<>) "Couldn't parse " . retext)

decodeRead' :: (Textual e, Readable a) => e -> FieldDecode e ByteString a
decodeRead' e = decodeReadWith (const e)

decodeReadWith :: (Textual e, FieldContents s, Semigroup e, Readable a) => (s -> e) -> FieldDecode e s a
decodeReadWith e = trimmed >>== \c ->
  maybe (badDecode (e c)) pure . fromBS . toByteString $ c

named :: (Readable a, FieldContents s, Textual e) => s -> FieldDecode e s a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap n . listToMaybe
      n'' = fromString (n' (toString name))
      space = " "
  in  decodeReadWith (\bs -> sconcat ("Couldn't parse \"" :| [retext bs, "\" as a", n'', space, retext name]))
