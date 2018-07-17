module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Data.Sv.Laws (test_Laws)

main :: IO ()
main =
  defaultMain $ testGroup "Tests" [
    test_Laws
  ]
