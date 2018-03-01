{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Sv.RoundTrips (test_Roundtrips) where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Hedgehog ((===), Property, Gen, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Parser.Char (CharParsing)
import Text.Trifecta (parseByteString)

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
import Data.Sv.Decode.Error (trifectaResultToEither)
import Data.Sv.Generators
import Data.Sv.Syntax (Sv, Headedness, SpacedField, comma)
import Data.Sv.Parse (defaultParseOptions, headedness, encodeString, separatedValues)
import Data.Sv.Parse.Internal (spacedField)
import Data.Sv.Print (defaultPrintOptions, printSvText)
import Data.Sv.Print.Internal (printSpaced)
import Text.Space (HorizontalSpace (Space, Tab), Spaces)
 
test_Roundtrips :: TestTree
test_Roundtrips =
  testGroup "Round trips" [
    csvRoundTrip
  , fieldRoundTrip
  , testGroup "decode/encode isomorphisms" [
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

printAfterParseRoundTrip :: (forall m. CharParsing m => m a) -> (a -> ByteString) -> TestName -> ByteString -> TestTree
printAfterParseRoundTrip parser display name s =
  testCase name $
    fmap display (trifectaResultToEither $ parseByteString parser mempty s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let sep = comma
      test =
        printAfterParseRoundTrip
        (spacedField sep UTF8.fromString :: CharParsing m => m (SpacedField ByteString))
        (BL.toStrict . Builder.toLazyByteString . printSpaced defaultPrintOptions)
  in  testGroup "field" [
    test "empty" ""
  , test "unquoted" "wobble"
  , test "unquoted with space" "  wiggle "
  , test "single quoted" "'tortoise'"
  , test "single quoted with space" " 'turtle'   "
  , test "single quoted with escape outer" "\'\'\'c\'\'\'"
  , test "single quoted with escape in the middle" "\'  The char \'\'c\'\' is nice.\'"
  , test "double quoted" "\"honey badger\""
  , test "double quoted with space" "   \" sun bear\" "
  , test "double quoted with escape outer" "\"\"\"laser\"\"\""
  , test "double quoted with escape in the middle" "\"John \"\"The Duke\"\" Wayne\""
  ]

csvRoundTrip :: TestTree
csvRoundTrip = testProperty "roundtrip" prop_csvRoundTrip

prop_csvRoundTrip :: Property
prop_csvRoundTrip =
  let genSpace :: Gen HorizontalSpace
      genSpace = Gen.element [Space, Tab]
      genSpaces :: Gen Spaces
      genSpaces = V.fromList <$> Gen.list (Range.linear 0 10) genSpace
      genText :: Gen Text
      genText  = Gen.text (Range.linear 1 100) Gen.alphaNum
      gen = genSvWithHeadedness (pure comma) genSpaces genText
      mkOpts h = defaultParseOptions & headedness .~ h & encodeString .~ Text.pack
      parseCsv :: CharParsing m => Headedness -> m (Sv Text)
      parseCsv = separatedValues . mkOpts
      parse h = parseByteString (parseCsv h) mempty
  in  property $ do
    (c,h) <- forAll gen
    trifectaResultToEither (fmap printSvText (parse h (printSvText c))) === pure (printSvText c)

encOpts :: EncodeOptions
encOpts = defaultEncodeOptions & quote .~ Nothing

parOpts :: ParseOptions ByteString
parOpts = defaultParseOptions & headedness .~ Unheaded

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form an isomorphism
roundTripCodecIso :: (Eq a, Show a) => TestName -> FieldDecode' ByteString a -> Encode a -> [(ByteString, a)] -> TestTree
roundTripCodecIso name dec enc bsas = testGroup name . flip foldMap bsas $ \(bs,a) ->
  [ testCase (UTF8.toString bs <> ": encode . decode") $
      Success (BL.fromStrict bs) @?= (encode enc encOpts <$> parseDecode dec parOpts bs)
  , testCase (UTF8.toString bs <> ": decode . encode") $
      Success [a] @?= (parseDecode dec parOpts $ BL.toStrict $ encodeRow enc encOpts a)
  ]

-- Round-trips an encode/decode pair. This version checks whether the pair
-- form a split-idempotent. That is to say, one direction is identity, the other is
-- idempotent.
roundTripCodecSplitIdempotent :: (Eq a, Show a) => TestName -> FieldDecode' ByteString a -> Encode a -> [(ByteString, a)] -> TestTree
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
