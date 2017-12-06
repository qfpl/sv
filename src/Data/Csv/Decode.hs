{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.Decode (
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
, choiceE
, orElse
, withDefault
, categorical
, decodeRead
, decodeRead'
, decodeReadWithMsg
, module Data.Csv.Decode.Error
, module Data.Csv.Decode.Field
, module Data.Csv.Decode.Row
, module Data.Csv.Decode.State
, module Data.Csv.Decode.Type
, FieldContents
) where

import Control.Lens (alaf)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (bimap, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First))
import Data.Readable (Readable (fromBS))
import Data.Set (Set, fromList, member)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT
import Text.Trifecta (Result, parseByteString, parseFromFileEx)

import Data.Csv.Csv (Csv, Headedness, recordList)
import Data.Csv.Decode.Error (DecodeError (UnknownCanonicalValue), DecodeValidation, bindValidation, badDecode, resultToDecodeError)
import Data.Csv.Decode.Field
import Data.Csv.Decode.Row
import Data.Csv.Decode.State
import Data.Csv.Decode.Type
import Data.Csv.Field (FieldContents)
import Data.Csv.Parser (separatedValues)
import Text.Babel (Textual, retext, showT, toByteString, toLazyByteString, toString, toText, trim)

decode :: RowDecode e s a -> Csv s -> DecodeValidation e [a]
decode r = traverse (runRowDecode r) . recordList

parseDecode ::
  Textual s
  => RowDecode e s a
  -> Headedness
  -> s
  -> Result (DecodeValidation e [a])
parseDecode d h =
  fmap (decode d) . parseByteString (separatedValues ',' h) mempty . toByteString

decodeFromFile ::
  (MonadIO m, Textual e, Textual s)
  => RowDecode e s a
  -> Headedness
  -> FilePath
  -> m (DecodeValidation e [a])
decodeFromFile d h fp =
  let decodeResult r = resultToDecodeError r `bindValidation` decode d
  in  decodeResult <$> parseFromFileEx (separatedValues ',' h) fp

contents :: FieldContents s => FieldDecode e s s
contents = fieldDecode pure

byteString :: FieldContents s => FieldDecode e s ByteString
byteString = toByteString <$> contents

utf8 :: IsString e => FieldDecode e ByteString Text
utf8 = contents >>==
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

choice :: FieldDecode e s a -> FieldDecode e s a -> FieldDecode e s a
choice = (<!>)

choiceE :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
choiceE a b = fmap Left a <!> fmap Right b

orElse :: FieldContents s => FieldDecode e s a -> a -> FieldDecode e s a
orElse f a = f <!> replace a

withDefault :: FieldContents s => FieldDecode e s b -> a -> FieldDecode e s (Either a b)
withDefault b a = swapE <$> choiceE b (replace a)
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
