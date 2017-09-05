{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Data.CSV.PrettyTest (test_Pretty) where

import Data.Text        (Text)
import Test.Tasty       (TestName, TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Parsec      (parse)
import Text.Parser.Char (CharParsing)

import Data.CSV.CSV     (CSV (CSV), final, noFinal)
import Data.CSV.Field   (unspacedField)
import Data.CSV.Parser  (comma, field, monoField)
import Data.CSV.Pretty  (prettyField, prettyCsv, prettyMonoField, setSeparator, textConfig)
import Data.CSV.Record  (NonEmptyRecord (SingleFieldNER))
import Text.Quote       (Quote (SingleQuote))

test_Pretty :: TestTree
test_Pretty =
  testGroup "Pretty" [
    fieldRoundTrip
  , csvPretty
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
      let subject = CSV comma mempty noFinal
      in  pretty subject @?= ""
  , testCase "empty quotes" $
      let subject = CSV comma mempty (final (SingleFieldNER (unspacedField SingleQuote "")))
      in pretty subject @?= "''"
  ]

