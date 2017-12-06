module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Csv.ParserTest (test_Parser)
import Data.Csv.PrettyTest (test_Pretty)
import Data.Csv.DecodeTest (test_Decode)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parser
  , test_Pretty
  , test_Decode
  ]

