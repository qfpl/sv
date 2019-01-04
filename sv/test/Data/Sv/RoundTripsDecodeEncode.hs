{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Sv.RoundTripsDecodeEncode (test_Roundtrips) where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup ((<>))
import Data.String (IsString)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E

test_Roundtrips :: TestTree
test_Roundtrips =
  testGroup "Round trips" [
    testGroup "decode/encode isomorphisms" [
      bool
    , char
    , int
    , integer
    , float
    , double
    , rational
    , readDouble
    , string
    , byteString
    , lazyByteString
    , text
    ]
  , testGroup "decode/encode normalising" [
      boolSI
    , floatSI
    , doubleSI
    ]

  ]

encOpts :: EncodeOptions
encOpts = defaultEncodeOptions & quoting .~ Never

parOpts :: ParseOptions
parOpts = defaultParseOptions & headedness .~ Unheaded

lbUtf8 :: LBS.ByteString -> String
lbUtf8 = UTF8.toString . LBS.toStrict

utf8lb :: String -> LBS.ByteString
utf8lb = LBS.fromStrict . UTF8.fromString

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form an isomorphism
roundTripCodecIso :: (Eq a, Show a) => TestName -> Decode' ByteString a -> Encode a -> [(LBS.ByteString, a)] -> TestTree
roundTripCodecIso name dec enc bsas = testGroup name . flip foldMap bsas $ \(bs,a) ->
  [ testCase (lbUtf8 bs <> ": encode . decode") $
      Success bs @?= (encode enc encOpts <$> parseDecode dec parOpts bs)
  , testCase (lbUtf8 bs <> ": decode . encode") $
      Success [a] @?= (parseDecode dec parOpts $ encodeRow enc encOpts a)
  ]

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form a split-idempotent. That is to say, one direction is identity, the other is
-- idempotent.
roundTripCodecSplitIdempotent :: (Eq a, Show a) => TestName -> Decode' ByteString a -> Encode a -> [(LBS.ByteString, a)] -> TestTree
roundTripCodecSplitIdempotent name dec enc bsas =
    let deco = parseDecode dec parOpts
        enco = encode enc encOpts
        encdec = fmap enco . deco
    in  testGroup name . flip foldMap bsas $ \(bs,a) ->
      [ testCase (lbUtf8 bs <> ": decode . encode . decode") $
          Success (Success [a]) @?= (deco <$> encdec bs)
      , testCase (lbUtf8 bs <> ": decode . encode") $
          Success [a] @?= (parseDecode dec parOpts $ enco [a])
      ]

byteString :: TestTree
byteString = roundTripCodecIso "bytestring" D.contents E.byteString
  [ ("hello","hello")]

lazyByteString :: TestTree
lazyByteString = roundTripCodecIso "lazy bytestring" D.lazyByteString E.lazyByteString [("hello","hello")]

bool :: TestTree
bool = roundTripCodecIso "bool" D.boolean E.booltruefalse [("true", True), ("false", False)]

char :: TestTree
char = roundTripCodecIso "char" D.char E.char [(utf8lb "c", 'c'), (utf8lb "💩", '💩')]

string :: TestTree
string = roundTripCodecIso "string" D.string E.string [(utf8lb "hello", "hello"), (utf8lb "💩💩💩💩", "💩💩💩💩")]

int :: TestTree
int = roundTripCodecIso "int" D.int E.int [("5", 5)]

integer :: TestTree
integer = roundTripCodecIso "integer" D.integer E.integer
  [ ("5", 5)
  , ("1000000", 1000000)
  ]

floatingTests :: (IsString s, Fractional a) => [(s,a)]
floatingTests =
  [ ("5.0", 5)
  , ("10.5", 10.5)
  , ("12345.678", 12345.678)
  ]

float :: TestTree
float = roundTripCodecIso "float" D.float E.float
  floatingTests

double :: TestTree
double = roundTripCodecIso "double" D.double E.double
  floatingTests

rational :: TestTree
rational = roundTripCodecIso "rational" D.rational E.double
  (("7.845860130857695", 7.845860130857695) : floatingTests)

readDouble :: TestTree
readDouble = roundTripCodecIso "read double" D.read (E.show :: Encode Double)
  ( ("1.0000000000034547e-2", 1.0000000000034547e-2)
  : ("7.845860130857695", 7.845860130857695)
  : floatingTests
  )

text :: TestTree
text = roundTripCodecIso "text" D.utf8 E.text [(utf8lb "hello", "hello"), (utf8lb "💩💩💩💩", "💩💩💩💩")]

boolSI :: TestTree
boolSI = roundTripCodecSplitIdempotent "bool" D.boolean E.bool10
  [ ("1", True)
  , ("0", False)
  ]

floatSI :: TestTree
floatSI = roundTripCodecSplitIdempotent "float" D.float E.float
  [ ("5", 5)
  ]

doubleSI :: TestTree
doubleSI = roundTripCodecSplitIdempotent "double" D.double E.double
  [ ("5", 5)
  ]
