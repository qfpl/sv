module Main (main) where

import Control.Applicative ((*>))
import Control.DeepSeq (NFData)
import Control.Lens
import Criterion.Main
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv as C
import Data.Csv.Parser as C
import Data.Vector (Vector)
import HaskellWorks.Data.Dsv.Lazy.Cursor (DsvCursor (..), makeCursor)
import System.Exit (exitFailure)

import Data.Sv (Validation (Failure, Success), DecodeValidation, ParseOptions, defaultParseOptions, headedness, Headedness (Unheaded), comma, parseDecode, parseDecodeFromDsvCursor)
import Data.Sv.Cassava (parseDecodeFromCassava)
import Data.Sv.Random

opts :: ParseOptions
opts = defaultParseOptions & headedness .~ Unheaded

failOnError :: Show e => DecodeValidation e a -> IO a
failOnError v = case v of
  Failure e -> do
      putStr "Sanity check failed: "
      print e
      exitFailure
  Success s -> pure s

failOnLeft :: Show s => Either s a -> IO a
failOnLeft = either (\s -> print s *> exitFailure) pure

sanitySv :: ByteString -> IO DsvCursor
sanitySv bs = do
  _ <- failOnError (parseDec bs)
  let s = parse bs
  _ <- failOnError (dec s)
  s `seq` pure s

sanityCassava :: ByteString -> IO Csv
sanityCassava bs = do
  s <- failOnLeft (parseCassava bs)
  _ <- failOnLeft (decCassava s)
  _ <- failOnLeft (parseDecCassava bs)
  s `seq` pure s

parse :: ByteString -> DsvCursor
parse = makeCursor comma . LBS.fromStrict

dec :: DsvCursor -> DecodeValidation ByteString [Row]
dec = parseDecodeFromDsvCursor rowDec opts

decCassava :: C.Csv -> Either String (Vector Row)
decCassava = runParser . traverse parseRecord

parseDec :: ByteString -> DecodeValidation ByteString [Row]
parseDec = parseDecode rowDec opts . LBS.fromStrict

parseCassava :: ByteString -> Either String C.Csv
parseCassava = A.parseOnly (C.csv C.defaultDecodeOptions)

parseDecCassava :: ByteString -> Either String (Vector Row)
parseDecCassava = C.decode C.NoHeader . LBS.fromStrict

parseCassavaDecSv :: ByteString -> DecodeValidation ByteString [Row]
parseCassavaDecSv = parseDecodeFromCassava rowDec Unheaded C.defaultDecodeOptions

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
  benchData <- loadOrCreateTestFiles
  cassavas <- traverse sanityCassava benchData
  svs <- traverse sanitySv benchData

  defaultMain
      [ mkBench "Parse hw-dsv" parse benchData
      , mkBench "Parse Cassava" parseCassava benchData
      , mkBench "Decode sv" dec svs
      , mkBench "Decode Cassava" decCassava cassavas
      , mkBench "Parse and decode sv" parseDec benchData
      , mkBench "Parse with Cassava, decode with sv" parseCassavaDecSv benchData
      , mkBench "Parse and decode Cassava" parseDecCassava benchData
      ]
