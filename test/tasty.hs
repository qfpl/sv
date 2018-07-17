module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.CassavaTest (test_CassavaAgreement)
import Data.Sv.DecodeTest (test_Decode)
import Data.Sv.EncodeTest (test_Encode)
import Data.Sv.Laws (test_Laws)
import Data.Sv.RoundTripsDecodeEncode (test_Roundtrips)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Decode
  , test_Encode
  , test_Roundtrips
  , test_CassavaAgreement
  , test_Laws
  ]
