{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.Csv.PrettyTest (test_Pretty) where

import Control.Lens         (view)
import Data.Text            (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text1           (Text1, packed1)
import Hedgehog             ((===), Property, Gen, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty           (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog  (testProperty)
import Test.Tasty.HUnit     ((@?=), testCase)
import Text.Parsec          (parse)
import Text.Parser.Char     (CharParsing)

import Data.Csv.Csv         (Csv (Csv))
import Data.Csv.Field       (unspacedField)
import Data.Csv.Generators  (genCsv)
import Data.Csv.Parser      (comma, monoField, separatedValues)
import Data.Csv.Pretty      (prettyCsv, defaultConfig, prettyMonoField, setSeparator, textConfig)
import Data.Csv.Record      (NonEmptyRecord (SingleFieldNER), final, noFinal)
import Text.Quote           (Quote (SingleQuote))

test_Pretty :: TestTree
test_Pretty =
  testGroup "Pretty" [
    fieldRoundTrip
  , csvPretty
  , csvRoundTrip
  ]

prettyAfterParseRoundTrip :: (forall m. CharParsing m  => m a) -> (a -> Text) -> TestName -> Text -> TestTree
prettyAfterParseRoundTrip parser pretty name s =
  testCase name $
    fmap pretty (parse parser "" s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let sep = comma
      config = setSeparator textConfig sep
      test = prettyAfterParseRoundTrip (monoField sep) (prettyMonoField config)
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
      let subject = Csv comma mempty noFinal
      in  pretty subject @?= ""
  , testCase "empty quotes" $
      let subject = Csv comma mempty (final (SingleFieldNER (unspacedField SingleQuote "")))
      in pretty subject @?= "''"
  ]

csvRoundTrip :: TestTree
csvRoundTrip = testProperty "roundtrip" prop_csvRoundTrip

prop_csvRoundTrip :: Property
prop_csvRoundTrip =
  let genSpace = Gen.text (Range.linear 0 5) (Gen.element [' ', '\t'])
      genText  = Gen.text (Range.linear 0 100) Gen.alphaNum
      genNonEmptyString = Gen.nonEmpty (Range.linear 0 100) Gen.alphaNum
      genText1 = view packed1 <$> genNonEmptyString
      gen = genCsv (pure ',') genSpace genText1 genText
      pretty = toLazyText . prettyCsv defaultConfig
      parseCsv :: (Monad m, CharParsing m) => m (Csv Text Text1 Text)
      parseCsv = separatedValues ','
  in  parsePretty parseCsv pretty gen

parsePretty :: (Eq a, Show a) => (forall m. (Monad m, CharParsing m) => m a) -> (a -> Lazy.Text) -> Gen a -> Property
parsePretty parser pretty genA =
  property $ do
    c <- forAll genA
    parse parser "" (pretty c) === pure c

