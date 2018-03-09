import Control.Applicative ((*>))
import Control.DeepSeq
import Control.Lens
import Criterion.Main
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv as C
import Data.Csv.Parser as C
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D
import Data.Sv.Random

opts :: ParseOptions ByteString
opts = defaultParseOptions & headedness .~ Unheaded

parseDec :: SvParser ByteString -> ByteString -> DecodeValidation ByteString [Row]
parseDec svp = D.parseDecode' svp rowDec opts

failOnError :: Show e => DecodeValidation e a -> IO a
failOnError v = case v of
  Failure e -> do
      putStr "Sanity check failed: "
      print e
      exitFailure
  Success s -> pure s

failOnLeft :: Show s => Either s a -> IO a
failOnLeft = either (\s -> print s *> exitFailure) pure

sanitySv :: SvParser ByteString -> ByteString -> IO (Sv ByteString)
sanitySv svp bs = do
  s <- failOnLeft (parse svp bs)
  _ <- failOnError (dec s)
  s `seq` pure s

sanityCassava :: ByteString -> IO ()
sanityCassava bs = do
  _ <- failOnLeft (parseCassava bs)
  _ <- failOnLeft (parseDecCassava bs)
  pure ()

parse :: SvParser ByteString -> ByteString -> Either ByteString (Sv ByteString)
parse svp = parseSv' svp opts

dec :: Sv ByteString -> DecodeValidation ByteString [Row]
dec = D.decode rowDec

parseCassava :: ByteString -> Either String C.Csv
parseCassava = A.parseOnly (C.csv C.defaultDecodeOptions)

parseDecCassava :: ByteString -> Either String (Vector Row)
parseDecCassava = C.decode C.NoHeader . BL.fromStrict

mkBench :: NFData b => String -> (a -> b) -> BenchData a -> Benchmark
mkBench nm func bs = bgroup nm [
      bench "1" $ nf func (f1 bs)
    , bench "10" $ nf func (f10 bs)
    , bench "100" $ nf func (f100 bs)
    , bench "500" $ nf func (f500 bs)
    , bench "1000" $ nf func (f1000 bs)
    , bench "5000" $ nf func (f5000 bs)
    , bench "10000" $ nf func (f10000 bs)
    , bench "50000" $ nf func (f50000 bs)
    , bench "100000" $ nf func (f100000 bs)
    ]

main :: IO ()
main = do
  traverse_ sanityCassava benchData
  traverse_ (sanitySv trifecta) benchData
  svs <- traverse (sanitySv attoparsecByteString) benchData

  defaultMain
      [ mkBench "Parse (Trifecta)" (parse trifecta) benchData
      , mkBench "Parse (Data.Attoparsec.ByteString)" (parse attoparsecByteString) benchData
      , mkBench "Parse Cassava" parseCassava benchData
      , mkBench "Decode" dec svs
      -- cassava does not seem to have a "decode only" option against which to compare
      , mkBench "Parse and decode (Trifecta)" (parseDec trifecta) benchData
      , mkBench "Parse and decode (Data.Attoparsec.ByteString)" (parseDec attoparsecByteString) benchData
      , mkBench "Parse and decode Cassava" parseDecCassava benchData
      ]
