import Control.Lens
import Data.Text (Text)
import System.Exit (exitFailure)
import Text.Trifecta (parseFromFile)

import Data.Sv
import Data.Sv.Parser (separatedValuesC)
import Data.Sv.Print (writeSvToFile)
import Text.Space (middle)
import Text.Quote (Quote (DoubleQuote), quote)

-- Manipulates the syntax directly with optics without decoding to data types
-- Rewrites a file with inconsistent quoting to consistently use double quotes

config :: SvConfig
config = defaultConfig

main :: IO ()
main = do
  svMay <- parseFromFile (separatedValuesC config) "test/requote.csv"
  case svMay of
    Nothing -> exitFailure
    Just s ->
      let s' :: Sv Text
          s' = fixQuotes s
      in  writeSvToFile "test/requote-fixed.csv" s'

fixQuotes :: Sv s -> Sv s
fixQuotes = over headerFields fixQuote . over recordFields fixQuote
  where
    fixQuote :: Field a -> Field a
    fixQuote f = case f of
      UnquotedF a -> unspacedField DoubleQuote a
      QuotedF sqs ->
        QuotedF (set (middle.quote) DoubleQuote sqs)
    headerFields = traverseHeader . traverseFields
    recordFields = traverseRecords . traverseFields
