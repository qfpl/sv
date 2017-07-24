module Data.CSV.ParserTest where

import           Data.Either      (isLeft)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase, (@?=))
import           Text.Parsec

import           Data.CSV.Parser

test_Parser :: TestTree
test_Parser =
  testGroup "Parser" [
    singleQuotedFieldTest
--  , doubleQuotedFieldTest
  ]

singleQuotedFieldTest :: TestTree
singleQuotedFieldTest =
  let p = parse singleQuotedField ""
  in testGroup "singleQuotedField" [
    testCase "pass" $
      p "'hello text'" @?= Right "hello text"
  , testCase "no closing quote" $
      assertBool "wasn't left" (isLeft (p "'no closing quote"))
  , testCase "no opening quote" $
      assertBool "wasn't left" (isLeft (p "no opening quote'"))
  , testCase "no quotes" $
      assertBool "wasn't left" (isLeft (p "no quotes"))
  ]

