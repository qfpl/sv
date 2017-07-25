{-# language RankNTypes #-}

module Data.CSV.ParserTest where

import           Data.Either      (isLeft)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import           Text.Parser.Char (CharParsing)
import           Text.Parsec

import           Data.CSV.Parser

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    singleQuotedFieldTest
  , doubleQuotedFieldTest
  , fieldTest
  , recordTest
  , separatedValuesTest
  ]

(@?=/) ::
  (Show a, Show b, Eq a, Eq b)
  => Either a b
  -> b
  -> Assertion
(@?=/) l r = l @?= Right r

quotedFieldTest :: (forall m . CharParsing m => m String) -> String -> String -> TestTree
quotedFieldTest parser name q =
  let p = parse parser "" . concat
  in testGroup name [
    testCase "pass" $
      p [q,"hello text",q] @?=/ "hello text"
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  , testCase "quoted field can handle escaped quotes" $
     p [q,"yes\\", q, "no", q] @?=/ concat ["yes", q, "no"]
  , testCase "quoted field can handle backslash on its own" $
     p [q,"hello\\goodbye",q] @?=/ "hello\\goodbye"
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" "'"
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" "\""

fieldTest :: TestTree
fieldTest =
  let p = parse (field comma) ""
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ "hello"
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ "goodbye"
  , testCase "unquoted" $
      p "yes" @?=/ "yes"
  , testCase "spaced doublequoted" $
     p "       \" spaces \"    " @?=/ " spaces "
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ " more spaces "
  , testCase "spaced unquoted" $
     p "  text  " @?=/ "  text  "
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ "hello,there,"
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ "court,of,the,,,,crimson,king"
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ "close"
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ ""
  ]

recordTest :: TestTree
recordTest =
  let p = parse (record comma) ""
  in  testGroup "record" [
    testCase "single field" $
      p "Yes" @?=/ ["Yes"]
  , testCase "fields" $
      p "Anderson,Squire,Wakeman,Howe,Bruford" @?=/ ["Anderson", "Squire", "Wakeman", "Howe", "Bruford"]
  , testCase "commas" $
      p ",,," @?=/ ["","","",""]
  , testCase "record ends at newline" $
      p "a,b,c\nd,e,f" @?=/ ["a","b","c"]
  , testCase "record ends at carriage return" $
      p "g,h,i\rj,k,l" @?=/ ["g","h","i"]
  , testCase "record ends at carriage return followed by newline" $
      p "m,n,o\r\np,q,r" @?=/ ["m","n","o"]
  ]

separatedValuesTest :: TestTree
separatedValuesTest =
  let p = parse (separatedValues comma) ""
  in  testGroup "separatedValue" [
    testCase "empty string" $
      p "" @?=/ []
  , testCase "single field, single record" $
      p "one" @?=/ [["one"]]
  , testCase "single field, multiple records" $
      p "one\nun" @?=/ [["one"],["un"]]
  , testCase "multiple fields, single record" $
      p "one,two" @?=/ [["one","two"]]
  , testCase "multiple fields, multiple records" $
      p "one,two,three\nun,deux,trois" @?=/ [["one", "two", "three"], ["un", "deux", "trois"]]
  ]

