{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.PrettyTest (test_Pretty) where

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

import Data.Sv.Sv          (Csv (Csv), Headedness, noHeader, comma)
import Data.Sv.Field       (unspacedField)
import Data.Sv.Generators  (genCsvWithHeadedness)
import Data.Sv.Parser.Internal (field, separatedValues)
import Data.Sv.Pretty      (prettyCsv, defaultConfig, setSeparator, textConfig)
import Data.Sv.Pretty.Internal (prettyField)
import Data.Sv.Record      (emptyRecords, singleton, singletonRecords)
import Text.Babel          (toByteString)
import Text.Space          (HorizontalSpace (Space, Tab))
import Text.Quote          (Quote (SingleQuote))

test_Pretty :: TestTree
test_Pretty =
  testGroup "Pretty" [
    fieldRoundTrip
  , csvPretty
  , csvRoundTrip
  ]

-- TODO dedupe this
r2e :: Result a -> Either String a
r2e r = case r of
  Success a -> Right a
  Failure e -> Left (show (_errDoc e))

prettyAfterParseRoundTrip :: (forall m. CharParsing m  => m a) -> (a -> ByteString) -> TestName -> ByteString -> TestTree
prettyAfterParseRoundTrip parser pretty name s =
  testCase name $
    fmap (toByteString . pretty) (r2e $ parseByteString parser mempty s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let sep = comma
      config = setSeparator textConfig sep
      test = prettyAfterParseRoundTrip (field sep) (toByteString . prettyField config)
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

csvPretty :: TestTree
csvPretty =
  let pretty = prettyCsv textConfig
  in  testGroup "csvPretty" [
    testCase "empty" $
      let subject = Csv comma noHeader emptyRecords []
      in  pretty subject @?= ""
  , testCase "empty quotes" $
      let subject = Csv comma noHeader (singletonRecords (singleton (unspacedField SingleQuote ""))) []
      in pretty subject @?= "''"
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
      genText  = Gen.text (Range.linear 0 100) Gen.alphaNum
      gen = genCsvWithHeadedness (pure comma) genSpaces genText
      pretty = toByteString . prettyCsv defaultConfig
      parseCsv :: CharParsing m => Headedness -> m (Csv Text)
      parseCsv = separatedValues comma
      parse h = parseByteString (parseCsv h) mempty
  in  property $ do
    (c,h) <- forAll gen
    r2e (fmap pretty (parse h (pretty c))) === pure (pretty c)
