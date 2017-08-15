{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad        (unless)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec          (parse)
import Text.Parser.Char     (CharParsing)
import System.Exit          (exitFailure)
import System.IO            (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)

import Data.CSV.Arbitraries (genCsv)
import Data.CSV.CSV         (CSV)
import Data.CSV.Parser      (separatedValues)
import Data.CSV.Pretty      (prettyCsv)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  result <- tests
  unless result exitFailure

tests :: IO Bool
tests =
  checkParallel $ Group "tests" [
    ("prop_csv", prop_csv)
  ]

prop_csv :: Property
prop_csv =
  let genSpace = Gen.string (Range.linear 0 5) (Gen.element [' ', '\t'])
      gen = genCsv (pure ',') genSpace (Gen.string (Range.linear 0 100) Gen.alphaNum)
      pretty = prettyCsv
      parseCsv :: CharParsing m => m (CSV String String)
      parseCsv = separatedValues ','
  in  parsePretty parseCsv pretty gen

parsePretty :: (Eq a, Show a) => (forall m. CharParsing m => m a) -> (a -> String) -> Gen a -> Property
parsePretty parser pretty genA =
  property $ do
    c <- forAll genA
    parse parser "" (pretty c) === pure c

