{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Sv.RoundTripsDecodeEncode (test_Roundtrips) where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup ((<>))
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
encOpts = defaultEncodeOptions & quote .~ Nothing

parOpts :: ParseOptions ByteString
parOpts = defaultParseOptions & headedness .~ Unheaded

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form an isomorphism
roundTripCodecIso :: (Eq a, Show a) => TestName -> Decode' ByteString a -> Encode a -> [(ByteString, a)] -> TestTree
roundTripCodecIso name dec enc bsas = testGroup name . flip foldMap bsas $ \(bs,a) ->
  [ testCase (UTF8.toString bs <> ": encode . decode") $
      Success (BL.fromStrict bs) @?= (encode enc encOpts <$> parseDecode dec parOpts bs)
  , testCase (UTF8.toString bs <> ": decode . encode") $
      Success [a] @?= (parseDecode dec parOpts $ BL.toStrict $ encodeRow enc encOpts a)
  ]

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form a split-idempotent. That is to say, one direction is identity, the other is
-- idempotent.
roundTripCodecSplitIdempotent :: (Eq a, Show a) => TestName -> Decode' ByteString a -> Encode a -> [(ByteString, a)] -> TestTree
roundTripCodecSplitIdempotent name dec enc bsas =
    let deco = parseDecode dec parOpts
        enco = encode enc encOpts
        encdec = fmap enco . deco
    in  testGroup name . flip foldMap bsas $ \(bs,a) ->
      [ testCase (UTF8.toString bs <> ": decode . encode . decode") $
          Success (Success [a]) @?= (deco . BL.toStrict <$> encdec bs)
      , testCase (UTF8.toString bs <> ": decode . encode") $
          Success [a] @?= (parseDecode dec parOpts $ BL.toStrict $ enco [a])
      ]

byteString :: TestTree
byteString = roundTripCodecIso "bytestring" D.contents E.byteString
  [ ("hello","hello")]

lazyByteString :: TestTree
lazyByteString = roundTripCodecIso "lazy bytestring" D.lazyByteString E.lazyByteString [("hello","hello")]

bool :: TestTree
bool = roundTripCodecIso "bool" D.boolean E.booltruefalse [("true", True), ("false", False)]

char :: TestTree
char = roundTripCodecIso "char" D.char E.char [(UTF8.fromString "c", 'c'), (UTF8.fromString "💩", '💩')]

string :: TestTree
string = roundTripCodecIso "string" D.string E.string [(UTF8.fromString "hello", "hello"), (UTF8.fromString "💩💩💩💩", "💩💩💩💩")]

int :: TestTree
int = roundTripCodecIso "int" D.int E.int [("5", 5)]

integer :: TestTree
integer = roundTripCodecIso "integer" D.integer E.integer
  [ ("5", 5)
  , ("1000000", 1000000)
  ]

float :: TestTree
float = roundTripCodecIso "float" D.float E.float
  [ ("5.0", 5)
  , ("10.5", 10.5)
  , ("12345.678", 12345.678)
  ]

double :: TestTree
double = roundTripCodecIso "double" D.double E.double [("5.0", 5)]

text :: TestTree
text = roundTripCodecIso "text" D.utf8 E.text [(UTF8.fromString "hello", "hello"), (UTF8.fromString "💩💩💩💩", "💩💩💩💩")]

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
