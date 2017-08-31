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
import Data.NonEmptyString  (NonEmptyString)

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
      genString = Gen.string (Range.linear 0 100) Gen.alphaNum
      genNonEmptyString = Gen.nonEmpty (Range.linear 0 100) Gen.alphaNum
      gen = genCsv (pure ',') genSpace genNonEmptyString genString
      pretty = prettyCsv
      parseCsv :: (Monad m, CharParsing m) => m (CSV String NonEmptyString String)
      parseCsv = separatedValues ','
  in  parsePretty parseCsv pretty gen

parsePretty :: (Eq a, Show a) => (forall m. (Monad m, CharParsing m) => m a) -> (a -> String) -> Gen a -> Property
parsePretty parser pretty genA =
  property $ do
    c <- forAll genA
    parse parser "" (pretty c) === pure c

