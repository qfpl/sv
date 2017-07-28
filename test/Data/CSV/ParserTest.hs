{-# language RankNTypes #-}

module Data.CSV.ParserTest (test_Parser) where

import           Data.Either      (isLeft)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import           Text.Parser.Char (CharParsing)
import           Text.Parsec

import           Data.CSV.Field
import           Data.CSV.Parser
import           Data.CSV.Record  (Record (Record))
import           Text.Between     (Between (Between), betwixt, uniform)
import           Text.Quote       (Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

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

qd = Quoted DoubleQuote
qs = Quoted SingleQuote
uq = UnquotedF
uqa = Record . fmap uq
uqaa = fmap uqa
nospc = QuotedF . uniform ""

quotedFieldTest :: (forall m . CharParsing m => m (Field String String)) -> String -> Quote -> TestTree
quotedFieldTest parser name quote =
  let p = parse parser "" . concat
      q = [quoteChar quote]
  in testGroup name [
    testCase "pass" $
      p [q,"hello text",q]
        @?=/ (nospc (Quoted quote "hello text"))
  , testCase "capture space" $
      p ["   ", q, " spaced text  ", q, "     "]
        @?=/ QuotedF (Between "   " "     " (Quoted quote " spaced text  "))
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  , testCase "quoted field can handle escaped quotes" $
     p [q,"yes", q, q, "no", q] @?=/ nospc (Quoted quote (concat ["yes", q, "no"]))
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" SingleQuote
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" DoubleQuote

fieldTest :: TestTree
fieldTest =
  let p = parse (field comma) ""
  in  testGroup "field" [
    testCase "doublequoted" $
      p "\"hello\"" @?=/ nospc (qd "hello")
  , testCase "singlequoted" $
      p "'goodbye'" @?=/ nospc (qs "goodbye")
  , testCase "unquoted" $
      p "yes" @?=/ uq "yes"
  , testCase "spaced doublequoted" $
     p "       \" spaces  \"    " @?=/ QuotedF (Between "       " "    " (qd " spaces  "))
  , testCase "spaced singlequoted" $
     p "        ' more spaces ' " @?=/ QuotedF (Between  "        " " " (qs " more spaces "))
  , testCase "spaced unquoted" $
     p "  text  " @?=/ uq "  text  "
  , testCase "fields can include the separator in single quotes" $
     p "'hello,there,'" @?=/ nospc (qs "hello,there,")
  , testCase "fields can include the separator in double quotes" $
     p "\"court,of,the,,,,crimson,king\"" @?=/ nospc (qd "court,of,the,,,,crimson,king")
  , testCase "unquoted fields stop at the separator (1)" $
     p "close,to,the,edge" @?=/ uq "close"
  , testCase "unquoted fields stop at the separator (2)" $
     p ",close,to,the,edge" @?=/ uq ""
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

