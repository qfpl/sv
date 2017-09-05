{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens         (view)
import Control.Monad        (unless)
import Data.Text            (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text1           (Text1, packed1)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec          (parse)
import Text.Parser.Char     (CharParsing)
import System.Exit          (exitFailure)
import System.IO            (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)

import Data.CSV.Generators  (genCsv)
import Data.CSV.CSV         (CSV)
import Data.CSV.Parser      (separatedValues)
import Data.CSV.Pretty      (prettyCsv, defaultConfig)

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
  let genSpace = Gen.text (Range.linear 0 5) (Gen.element [' ', '\t'])
      genText  = Gen.text (Range.linear 0 100) Gen.alphaNum
      genNonEmptyString = Gen.nonEmpty (Range.linear 0 100) Gen.alphaNum
      genText1 = view packed1 <$> genNonEmptyString
      gen = genCsv (pure ',') genSpace genText1 genText
      pretty = toLazyText . prettyCsv defaultConfig
      parseCsv :: (Monad m, CharParsing m) => m (CSV Text Text1 Text)
      parseCsv = separatedValues ','
  in  parsePretty parseCsv pretty gen

parsePretty :: (Eq a, Show a) => (forall m. (Monad m, CharParsing m) => m a) -> (a -> Lazy.Text) -> Gen a -> Property
parsePretty parser pretty genA =
  property $ do
    c <- forAll genA
    parse parser "" (pretty c) === pure c

