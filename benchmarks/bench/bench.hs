import Control.Lens
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Sv
import qualified Data.Sv.Decode as D
import Data.Sv.Random

loadFile :: Int -> IO ByteString
loadFile = BS.readFile . (\n -> "benchmarks/csv/" ++ show n ++ ".csv")

opts :: ParseOptions
opts = defaultParseOptions & headedness .~ Unheaded

dec :: BS.ByteString -> DecodeValidation ByteString [Row]
dec = D.parseDecode rowDec (Just opts)

main :: IO ()
main = do
  f1 <- loadFile 1
  f10 <- loadFile 10
  f100 <- loadFile 100
  f500 <- loadFile 500
  f1000 <- loadFile 1000
  f5000 <- loadFile 5000
  f10000 <- loadFile 10000
  f50000 <- loadFile 50000
  f100000 <- loadFile 100000

  print (dec f10)

  f1 `seq` f10 `seq` f100 `seq` f500 `seq` f1000 `seq` f5000 `seq` f10000 `seq` f50000 `seq` f100000
    `seq` defaultMain
      [ bgroup "decode" [
          bench "1" $ whnf dec f1
        , bench "10" $ whnf dec f10
        , bench "100" $ whnf dec f100
        , bench "500" $ whnf dec f500
        , bench "1000" $ whnf dec f1000
        , bench "5000" $ whnf dec f5000
        , bench "10000" $ whnf dec f10000
        , bench "50000" $ whnf dec f50000
        , bench "100000" $ whnf dec f100000
        ]
      ]
