{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.PrintTest (test_Print) where

import Control.Lens         ((&), (.~))
import Data.ByteString      (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import Data.Text            (Text)
import qualified Data.Text as Text
import Hedgehog             ((===), Property, Gen, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty           (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog  (testProperty)
import Test.Tasty.HUnit     ((@?=), testCase)
import Text.Parser.Char     (CharParsing)
import Text.Trifecta        (Result (Success, Failure), parseByteString, _errDoc)

import Data.Sv.Generators  (genSvWithHeadedness)
import Data.Sv.Parse       (defaultParseOptions, headedness, encodeString)
import Data.Sv.Parse.Internal (spacedField, separatedValues)
import Data.Sv.Print       (printSv, printSvText)
import Data.Sv.Print.Internal (printSpaced)
import Data.Sv.Syntax.Field (Field (Quoted), SpacedField)
import Data.Sv.Syntax.Record (Records (EmptyRecords), singleField, singleRecord)
import Data.Sv.Syntax.Sv   (Sv (Sv), Headedness, noHeader, comma)
import Text.Escape         (escapeUtf8)
import Text.Space          (HorizontalSpace (Space, Tab))
import Text.Quote          (Quote (SingleQuote))

test_Print :: TestTree
test_Print =
  testGroup "Print" [
    fieldRoundTrip
  , csvPrint
  , csvRoundTrip
  ]

-- TODO dedupe this
r2e :: Result a -> Either String a
r2e r = case r of
  Success a -> Right a
  Failure e -> Left (show (_errDoc e))

printAfterParseRoundTrip :: (forall m. CharParsing m => m a) -> (a -> ByteString) -> TestName -> ByteString -> TestTree
printAfterParseRoundTrip parser display name s =
  testCase name $
    fmap display (r2e $ parseByteString parser mempty s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let sep = comma
      test =
        printAfterParseRoundTrip
        (spacedField sep UTF8.fromString :: CharParsing m => m (SpacedField ByteString))
        (BL.toStrict . Builder.toLazyByteString . printSpaced escapeUtf8 Builder.byteString)
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

csvPrint :: TestTree
csvPrint =
  testGroup "csvPrint" [
    testCase "empty" $
      let subject :: Sv ByteString
          subject = Sv comma noHeader EmptyRecords []
      in  printSv subject @?= ""
  , testCase "empty quotes" $
      let subject :: Sv ByteString
          subject = Sv comma noHeader (singleRecord (singleField (Quoted SingleQuote mempty))) []
      in printSv subject @?= ("''" :: ByteString)
  ]

csvRoundTrip :: TestTree
csvRoundTrip = testProperty "roundtrip" prop_csvRoundTrip

prop_csvRoundTrip :: Property
prop_csvRoundTrip =
  let genSpace :: Gen HorizontalSpace
      genSpace = Gen.element [Space, Tab]
      genSpaces :: Gen [HorizontalSpace]
      genSpaces = Gen.list (Range.linear 0 10) genSpace
      genText :: Gen Text
      genText  = Gen.text (Range.linear 1 100) Gen.alphaNum
      gen = genSvWithHeadedness (pure comma) genSpaces genText
      mkOpts h = defaultParseOptions & headedness .~ h & encodeString .~ Text.pack
      parseCsv :: CharParsing m => Headedness -> m (Sv Text)
      parseCsv = separatedValues . mkOpts
      parse h = parseByteString (parseCsv h) mempty
  in  property $ do
    (c,h) <- forAll gen
    r2e (fmap printSvText (parse h (printSvText c))) === pure (printSvText c)
