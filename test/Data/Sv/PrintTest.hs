{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.PrintTest (test_Print) where

import Data.ByteString      (ByteString)
import Data.Text            (Text)
import Hedgehog             ((===), Property, Gen, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty           (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog  (testProperty)
import Test.Tasty.HUnit     ((@?=), testCase)
import Text.Parser.Char     (CharParsing)
import Text.Trifecta        (Result (Success, Failure), parseByteString, _errDoc)

import Data.Sv.Sv          (Sv (Sv), Headedness, noHeader, comma)
import Data.Sv.Field       (Field, unspacedField)
import Data.Sv.Generators  (genSvWithHeadedness)
import Data.Sv.Parser.Internal (field, separatedValues)
import Data.Sv.Print       (displaySv, printField)
import Data.Sv.Record      (emptyRecords, singleField, singleRecord)
import Text.Babel          (toByteString)
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

printAfterParseRoundTrip :: (forall m. CharParsing m  => m a) -> (a -> ByteString) -> TestName -> ByteString -> TestTree
printAfterParseRoundTrip parser display name s =
  testCase name $
    fmap display (r2e $ parseByteString parser mempty s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let sep = comma
      test = printAfterParseRoundTrip (field sep :: CharParsing m => m (Field ByteString)) (toByteString . printField)
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
          subject = Sv comma noHeader emptyRecords []
      in  displaySv subject @?= ""
  , testCase "empty quotes" $
      let subject = Sv comma noHeader (singleRecord (singleField (unspacedField SingleQuote []))) []
      in displaySv subject @?= "''"
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
      parseCsv :: CharParsing m => Headedness -> m (Sv Text)
      parseCsv = separatedValues comma
      parse h = parseByteString (parseCsv h) mempty
  in  property $ do
    (c,h) <- forAll gen
    r2e (fmap displaySv (parse h (displaySv c))) === pure (displaySv c)
