module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.ParseTest (test_Parse)
import Data.Sv.PrintTest (test_Print)
import Data.Sv.RoundTripsParsePrint (test_Roundtrips)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parse
  , test_Print
  , test_Roundtrips
  ]
