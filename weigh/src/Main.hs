module Main (main) where

import qualified Weigh as W
import qualified Data.ByteString as BS
import Data.Monoid (Sum (Sum, getSum))
import Data.Sv
import qualified Data.Sv.Decode as D
import System.Directory (doesFileExist)

filepath1 :: FilePath
filepath1 = "examples/csv/species.csv"

filepath2 :: FilePath
filepath2 = "../" ++ filepath1

main :: IO ()
main = do
  b <- doesFileExist filepath1
  let
    filepath = if b then filepath1 else filepath2
    loadFile  = parseDecodeFromFile D.row defaultParseOptions filepath
    sumLen = fmap (getSum . (foldMap . foldMap . foldMap) (Sum . BS.length)) loadFile
  W.mainWith $ do
    W.setColumns [W.Case, W.Allocated, W.GCs, W.Live, W.Check, W.Max]
    W.action "load file" loadFile
    W.action "load file and sum lengths" sumLen
