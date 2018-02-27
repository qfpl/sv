module Data.Sv.Example.Requote where

import Control.Lens
import Control.Monad (unless)
import Data.ByteString (ByteString)
import System.Exit (exitFailure)
import qualified Text.Trifecta as T (parseFromFile)

import Data.Sv
import Data.Sv.Parse (separatedValues)
import Data.Sv.Print (writeSvToFile)
import Text.Escape (Unescaped (Unescaped))
import Text.Quote (Quote (DoubleQuote))

original :: FilePath
original = "csv/requote.csv"

fixed :: FilePath
fixed = "csv/requote.fixed.csv"

golden :: FilePath
golden = "csv/requote.golden.csv"

main :: IO ()
main = do
  requote
  expected <- readFile golden
  actual <- readFile fixed
  unless (expected == actual)
    exitFailure

-- Manipulates the syntax directly with optics without decoding to data types
-- Rewrites a file with inconsistent quoting to consistently use double quotes

opts :: ParseOptions ByteString
opts = defaultParseOptions

requote :: IO ()
requote = do
  svMay <- T.parseFromFile (separatedValues opts) original
  case svMay of
    Nothing -> exitFailure
    Just s ->
      let s' :: Sv ByteString
          s' = fixQuotes s
      in  writeSvToFile fixed s'

fixQuotes :: Sv s -> Sv s
fixQuotes = over headerFields fixQuote . over recordFields fixQuote
  where
    fixQuote :: Field a -> Field a
    fixQuote f = case f of
      Unquoted a -> Quoted DoubleQuote (Unescaped a)
      Quoted _ v -> Quoted DoubleQuote v
    headerFields = traverseHeader . fields
    recordFields = traverseRecords . fields
