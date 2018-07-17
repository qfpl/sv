module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.Core.Laws (test_Laws)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Laws
  ]
