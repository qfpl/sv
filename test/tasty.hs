module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.ParserTest (test_Parser)
import Data.Sv.DecodeTest (test_Decode)
import Data.Sv.EncodeTest (test_Encode)
import Data.Sv.PrintTest  (test_Print)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parser
  , test_Print
  , test_Decode
  , test_Encode
  ]
