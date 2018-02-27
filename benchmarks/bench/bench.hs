{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative ((*>))
import Control.DeepSeq
import Control.Lens
import Criterion.Main
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Csv as C
import Data.Csv.Parser as C
import Data.Vector (Vector)
import GHC.Generics
import System.Exit (exitFailure)

import Data.Sv
import qualified Data.Sv.Decode as D
import Data.Sv.Random

data BenchData a =
  BenchData {
    f1 :: a
  , f10 :: a
  , f100 :: a
  , f500 :: a
  , f1000 :: a
  , f5000 :: a
  , f10000 :: a
  , f50000 :: a
  , f100000 :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (BenchData a) where

inds :: BenchData Int
inds = BenchData 1 10 100 500 1000 5000 10000 50000 100000

loadFile :: String -> Int -> IO ByteString
loadFile subdir n = BS.readFile ("benchmarks/csv/" ++ subdir ++ "/" ++ show n ++ ".csv")

-- subdirs
easy, hard :: String
easy = "easy"
hard = "hard"

bdFs :: String -> IO (BenchData ByteString)
bdFs subdir = traverse (loadFile subdir) inds

bdFs' :: String -> IO (BenchData (Sv ByteString))
bdFs' subdir = traverse sanitySv =<< bdFs subdir

opts :: ParseOptions ByteString
opts = defaultParseOptions & headedness .~ Unheaded

parseDec :: ByteString -> DecodeValidation ByteString [Row]
parseDec = D.parseDecode' attoparsecByteString rowDec opts

failOnError :: Show e => DecodeValidation e a -> IO a
failOnError v = case v of
  Failure e -> do
      putStr "Sanity check failed: "
      print e
      exitFailure
  Success s -> pure s

failOnLeft :: Show s => Either s b -> IO b
failOnLeft = either (\s -> print s *> exitFailure) pure

sanitySv :: ByteString -> IO (Sv ByteString)
sanitySv bs = do
  s <- failOnLeft (parse bs)
  _ <- failOnError (dec s)
  s `seq` pure s

sanityCassava :: ByteString -> IO ()
sanityCassava bs = do
  _ <- failOnLeft (parseCassava bs)
  _ <- failOnLeft (parseDecCassava bs)
  pure ()

parse :: ByteString -> Either ByteString (Sv ByteString)
parse = parseSv' attoparsecByteString opts

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
  easies <- bdFs easy
  hards <- bdFs hard
  traverse_ sanitySv easies
  traverse_ sanitySv hards
  traverse_ sanityCassava easies
  defaultMain
      [ env (bdFs  easy) $ mkBench "Parse" parse
      , env (bdFs  easy) $ mkBench "Parse Cassava" parseCassava
      , env (bdFs' easy) $ mkBench "Decode" dec
      -- cassava does not seem to have a "decode only" option against which to compare
      , env (bdFs  easy) $ mkBench "Parse and decode" parseDec
      , env (bdFs  easy) $ mkBench "Parse and decode Cassava" parseDecCassava

      -- These "hard" benchmarks are the RFC 1480 non-compliant versions.
      -- They cannot be compared to cassava as it can't parse them.
      , env (bdFs  hard) $ mkBench "Parse (hard mode)" parse
      , env (bdFs' hard) $ mkBench "Decode (hard mode)" dec
      , env (bdFs  hard) $ mkBench "Parse and decode (hard mode)" parseDec
      ]
