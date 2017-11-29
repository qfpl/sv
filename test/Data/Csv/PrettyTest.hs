{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.Csv.PrettyTest (test_Pretty) where

import Control.Lens         (view)
import Data.List.NonEmpty   (NonEmpty)
import Data.Text            (Text)
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

import Data.Csv.Csv         (Csv (Csv), Headedness, noHeader, comma)
import Data.Csv.Field       (unspacedField)
import Data.Csv.Generators  (genCsvWithHeadedness)
import Data.Csv.Parser.Internal (field, separatedValues)
import Data.Csv.Pretty      (prettyCsv, defaultConfig, setSeparator, textConfig)
import Data.Csv.Pretty.Internal (prettyField)
import Data.Csv.Record      (NonEmptyRecord (SingleFieldNER), final, noFinal)
import Text.Space           (HorizontalSpace (Space, Tab))
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
      test = prettyAfterParseRoundTrip (field sep) (prettyField config)
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
      let subject = Csv comma noHeader mempty noFinal
      in  pretty subject @?= ""
  , testCase "empty quotes" $
      let subject = Csv comma noHeader mempty (final (SingleFieldNER (unspacedField SingleQuote "")))
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
      genNonEmptyString :: Gen (NonEmpty Char)
      genNonEmptyString = Gen.nonEmpty (Range.linear 0 100) Gen.alphaNum
      genText1 :: Gen Text1
      genText1 = view packed1 <$> genNonEmptyString
      gen = genCsvWithHeadedness (pure comma) genSpaces genText1 genText
      pretty = toLazyText . prettyCsv defaultConfig
      parseCsv :: (Monad m, CharParsing m) => Headedness -> m (Csv Text1 Text)
      parseCsv = separatedValues comma
  in  property $ do
    (c,h) <- forAll gen
    parse (parseCsv h) "" (pretty c) === pure c
