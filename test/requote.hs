import Control.Lens
import Data.Text (Text)
import System.Exit (exitFailure)
import Text.Trifecta (parseFromFile)

import Test.Tasty (defaultMain)
import Test.Tasty.Golden (goldenVsFile)

import Data.Sv
import Data.Sv.Parser (separatedValuesC)
import Data.Sv.Print (writeSvToFile)
import Text.Escape (Unescaped (Unescaped))
import Text.Quote (Quote (DoubleQuote))

fixed :: FilePath
fixed = "test/requote.fixed.csv"

golden :: FilePath
golden = "test/requote.golden.csv"

main :: IO ()
main = defaultMain $
  goldenVsFile "requote" golden fixed requote

-- Manipulates the syntax directly with optics without decoding to data types
-- Rewrites a file with inconsistent quoting to consistently use double quotes

config :: SvConfig
config = defaultConfig

requote :: IO ()
requote = do
  svMay <- parseFromFile (separatedValuesC config) "test/requote.csv"
  case svMay of
    Nothing -> exitFailure
    Just s ->
      let s' :: Sv Text
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
