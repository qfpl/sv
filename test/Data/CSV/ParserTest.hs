{-# language RankNTypes #-}

module Data.CSV.ParserTest (test_Parser) where

import           Data.Either      (isLeft)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import           Text.Parser.Char (CharParsing)
import           Text.Parsec

import           Data.CSV.Field
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

singleQ = '\''
doubleQ = '"'
qd = Quoted doubleQ
qs = Quoted singleQ
uq = Unquoted
uqa = fmap Unquoted
uqaa = fmap (fmap Unquoted)

quotedFieldTest :: (forall m . CharParsing m => m Field) -> String -> Char -> TestTree
quotedFieldTest parser name qc =
  let p = parse parser "" . concat
      q = [qc]
  in testGroup name [
    testCase "pass" $
      p [q,"hello text",q] @?=/ Quoted qc "hello text"
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  , testCase "quoted field can handle escaped quotes" $
     p [q,"yes\\", q, "no", q] @?=/ Quoted qc (concat ["yes", q, "no"])
  , testCase "quoted field can handle backslash on its own" $
     p [q,"hello\\goodbye",q] @?=/ Quoted qc "hello\\goodbye"
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" singleQ
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" doubleQ

fieldTest :: TestTree
fieldTest =
  let p = parse (field comma) ""
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ qd "hello"
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ qs "goodbye"
  , testCase "unquoted" $
      p "yes" @?=/ uq "yes"
  , testCase "spaced doublequoted" $
     p "       \" spaces \"    " @?=/ qd " spaces "
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ qs " more spaces "
  , testCase "spaced unquoted" $
     p "  text  " @?=/ uq "  text  "
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ qs "hello,there,"
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ qd "court,of,the,,,,crimson,king"
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ uq "close"
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ uq ""
  , testCase "unquoted fields can contain escaped commas" $
     p "I knew him\\, Horatio" @?=/ uq "I knew him, Horatio"
  ]

recordTest :: TestTree
recordTest =
  let p = parse (record comma) ""
  in  testGroup "record" [
    testCase "single field" $
      p "Yes" @?=/ uqa ["Yes"]
  , testCase "fields" $
      p "Anderson,Squire,Wakeman,Howe,Bruford" @?=/ uqa ["Anderson", "Squire", "Wakeman", "Howe", "Bruford"]
  , testCase "commas" $
      p ",,," @?=/ uqa ["","","",""]
  , testCase "record ends at newline" $
      p "a,b,c\nd,e,f" @?=/ uqa ["a","b","c"]
  , testCase "record ends at carriage return" $
      p "g,h,i\rj,k,l" @?=/ uqa ["g","h","i"]
  , testCase "record ends at carriage return followed by newline" $
      p "m,n,o\r\np,q,r" @?=/ uqa ["m","n","o"]
  ]

separatedValuesTest :: TestTree
separatedValuesTest =
  let p = parse (separatedValues comma) ""
  in  testGroup "separatedValue" [
    testCase "empty string" $
      p "" @?=/ []
  , testCase "single field, single record" $
      p "one" @?=/ uqaa [["one"]]
  , testCase "single field, multiple records" $
      p "one\nun" @?=/ uqaa [["one"],["un"]]
  , testCase "multiple fields, single record" $
      p "one,two" @?=/ uqaa [["one","two"]]
  , testCase "multiple fields, multiple records" $
      p "one,two,three\nun,deux,trois"
        @?=/ uqaa [["one", "two", "three"], ["un", "deux", "trois"]]
  ]

