module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.DecodeTest (test_Decode)
import Data.Sv.EncodeTest (test_Encode)
import Data.Sv.ParseTest (test_Parse)
import Data.Sv.PrintTest (test_Print)
import Data.Sv.RoundTrips (test_Roundtrips)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parse
  , test_Print
  , test_Decode
  , test_Encode
  , test_Roundtrips
  ]
