module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.ParserTest (test_Parser)
import Data.Sv.PrettyTest (test_Pretty)
import Data.Sv.DecodeTest (test_Decode)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Parser
  , test_Pretty
  , test_Decode
  ]

