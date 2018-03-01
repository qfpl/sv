{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.PrintTest (test_Print) where

import Data.ByteString (ByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.Sv.Print (printSv)
import Data.Sv.Syntax.Field (Field (Quoted))
import Data.Sv.Syntax.Record (Records (EmptyRecords), singleField, singleRecord)
import Data.Sv.Syntax.Sv (Sv (Sv), noHeader, comma)
import Text.Quote (Quote (SingleQuote))

test_Print :: TestTree
test_Print =
  testGroup "Print" [
    csvPrint
  ]

csvPrint :: TestTree
csvPrint =
  testGroup "csvPrint" [
    testCase "empty" $
      let subject :: Sv ByteString
          subject = Sv comma noHeader EmptyRecords []
      in  printSv subject @?= ""
  , testCase "empty quotes" $
      let subject :: Sv ByteString
          subject = Sv comma noHeader (singleRecord (singleField (Quoted SingleQuote mempty))) []
      in printSv subject @?= ("''" :: ByteString)
  ]
