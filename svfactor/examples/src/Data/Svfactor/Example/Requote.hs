module Data.Svfactor.Example.Requote where

import Control.Lens
import Control.Monad (unless)
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Data.Svfactor.Parse (ParseOptions, defaultParseOptions, parseSvFromFile)
import Data.Svfactor.Print (writeSvToFile)
import Data.Svfactor.Syntax
import Data.Svfactor.Text.Escape (Unescaped (Unescaped))
import Data.Svfactor.Text.Quote (Quote (DoubleQuote))

-- Manipulates the syntax directly with optics without decoding to data types
-- Rewrites a file with inconsistent quoting to consistently use double quotes

original :: FilePath
original = "csv/requote.csv"

fixed :: FilePath
fixed = "csv/requote.fixed.csv"

golden :: FilePath
golden = "csv/requote.golden.csv"

opts :: ParseOptions ByteString
opts = defaultParseOptions

fixQuotes :: Sv s -> Sv s
fixQuotes = over headerFields fixQuote . over recordFields fixQuote
  where
    fixQuote :: Field a -> Field a
    fixQuote f = case f of
      Unquoted a -> Quoted DoubleQuote (Unescaped a)
      Quoted _ v -> Quoted DoubleQuote v
    headerFields = traverseHeader . fields
    recordFields = traverseRecords . fields

main :: IO ()
main = do
  requote
  expected <- readFile golden
  actual <- readFile fixed
  unless (expected == actual)
    exitFailure

requote :: IO ()
requote = do
  svEither <- parseSvFromFile opts original
  case svEither of
    Left _ -> exitFailure
    Right s ->
      let s' :: Sv ByteString
          s' = fixQuotes s
      in  writeSvToFile fixed s'
