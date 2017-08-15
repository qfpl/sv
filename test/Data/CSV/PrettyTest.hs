{-# language RankNTypes #-}

module Data.CSV.PrettyTest (test_Pretty) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Parsec      (parse)
import Text.Parser.Char (CharParsing)

import Data.CSV.Parser  (comma, field)
import Data.CSV.Pretty  (prettyField)

test_Pretty :: TestTree
test_Pretty =
  testGroup "Pretty" [
    fieldRoundTrip
  , recordRoundTrip
  ]

prettyAfterParseRoundTrip :: (forall m. CharParsing m  => m a) -> (a -> String) -> String -> String -> TestTree
prettyAfterParseRoundTrip parser pretty name s =
  testCase name $
    fmap pretty (parse parser "" s) @?= Right s

fieldRoundTrip :: TestTree
fieldRoundTrip =
  let test = prettyAfterParseRoundTrip (field comma) prettyField
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

recordRoundTrip :: TestTree
recordRoundTrip =
  testGroup "record" [
  ]

{-
parseAfterPrettyRoundTrip ::
  (Arbitrary a, Show a, Eq a) => (forall m. CharParsing m => m a) -> (a -> String) -> String -> TestTree
parseAfterPrettyRoundTrip parser pretty name =
  QC.testProperty name $
    \a -> parse parser "" (pretty a) == Right a

csvRoundTrip :: TestTree
csvRoundTrip =
  parseAfterPrettyRoundTrip (ArbCsv <$> separatedValues comma) (prettyCsv . unArbCsv) "csv"
-}

