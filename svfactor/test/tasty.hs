module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Svfactor.ParseTest (test_Parse)
import Data.Svfactor.PrintTest (test_Print)
import Data.Svfactor.RoundTripsParsePrint (test_Roundtrips)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parse
  , test_Print
  , test_Roundtrips
  ]
