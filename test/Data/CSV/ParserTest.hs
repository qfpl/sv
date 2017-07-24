{-# language RankNTypes #-}

module Data.CSV.ParserTest where

import           Data.Either      (isLeft)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase, (@?=))
import           Text.Parser.Char (CharParsing)
import           Text.Parsec

import           Data.CSV.Parser

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    singleQuotedFieldTest
  , doubleQuotedFieldTest
  ]

quotedFieldTest :: (forall m . CharParsing m => m String) -> String -> String -> TestTree
quotedFieldTest parser name q =
  let p = parse parser "" . concat
  in testGroup name [
    testCase "pass" $
      p [q,"hello text",q] @?= Right "hello text"
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p [q, "no closing quote"   ]))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p [   "no opening quote", q]))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p [   "no quotes"          ]))
  ]

singleQuotedFieldTest, doubleQuotedFieldTest :: TestTree
singleQuotedFieldTest = quotedFieldTest singleQuotedField "singleQuotedField" "'"
doubleQuotedFieldTest = quotedFieldTest doubleQuotedField "doubleQuotedField" "\""

