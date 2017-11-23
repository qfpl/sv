{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.Decode.ByteString where

import Control.Lens.Wrapped
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (toUpper)
import Data.Functor.Alt ((<!>))
import Data.Maybe (listToMaybe)
import Data.Monoid (First (First))
import Data.Semigroup (Semigroup ((<>)))
import Data.Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (decodeUtf8')
import Text.Read (readMaybe)

import Data.Csv.Field (FieldContents)
import Data.Csv.Decode (FieldDecode, contentsD, decodeMay', (>>==))
import Data.Csv.Decode.Error

byteString :: Semigroup e => FieldDecode e ByteString ByteString
byteString = contentsD

lazyByteString :: Semigroup e => FieldDecode e ByteString LBS.ByteString
lazyByteString = LBS.fromStrict <$> contentsD

string :: Semigroup e => FieldDecode e ByteString String
string = unpack <$> contentsD

text :: FieldDecode ByteString ByteString Text
text = contentsD >>==
  either (badDecode . pack . show) pure . decodeUtf8'

lazyText :: FieldDecode ByteString ByteString LT.Text
lazyText = LT.fromStrict <$> text

unit :: FieldDecode ByteString ByteString ()
unit = pure ()

int :: FieldDecode ByteString ByteString Int
int = named "int"

integer :: FieldDecode ByteString ByteString Integer
integer = named "integer"

float :: FieldDecode ByteString ByteString Float
float = named "float"

double :: FieldDecode ByteString ByteString Double
double = named "double"

eitherD :: FieldDecode e s a -> FieldDecode e s b -> FieldDecode e s (Either a b)
eitherD a b = fmap Left a <!> fmap Right b

withDefault :: Semigroup e => FieldDecode e s b -> a -> FieldDecode e s (Either a b)
withDefault b a = eitherD (pure a) b

categorical :: forall a s. (FieldContents s, Ord s) => [(a, [s])] -> FieldDecode ByteString s a
categorical as =
  let as' :: [(a, Set s)]
      as' = fmap (second fromList) as
      go :: s -> (a, Set s) -> Maybe a
      go s (a, set) =
        if s `member` set
        then Just a
        else Nothing
  in  contentsD >>== \s -> decodeMay' undefined $ alaf First foldMap (go s) as'

decodeRead :: Read a => FieldDecode ByteString ByteString a
decodeRead = decodeReadWith ("Couldn't parse " <>)

decodeRead' :: (Semigroup e, Read a) => e -> FieldDecode e ByteString a
decodeRead' e = decodeReadWith (const e)

decodeReadWith :: (Semigroup e, Read a) => (ByteString -> e) -> FieldDecode e ByteString a
decodeReadWith e = contentsD >>== \bs -> 
  maybe (badDecode (e bs)) pure . readMaybe . unpack $ bs

named :: Read a => ByteString -> FieldDecode ByteString ByteString a
named name =
  let vs' = ['a','e','i','o','u']
      vs  = fmap toUpper vs' ++ vs'
      n c = if c `elem` vs then "n" else ""
      n' = foldMap n . listToMaybe
      n'' = fromString (n' (unpack name))
      space = " "
  in  decodeReadWith (\bs -> mconcat ["Couldn't parse \"", bs, "\" as a", n'', space, name])
