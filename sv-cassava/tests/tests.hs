{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.Csv as Cassava
import Data.Sv
import Data.Sv.Cassava
import qualified Data.Sv.Decode as D
import Data.Text (Text)
import qualified Data.Vector as V
import System.Exit (exitFailure)
import Test.HUnit

data TestRow = TestRow Int Double Text deriving (Eq, Show)

testRowDec :: Decode' ByteString TestRow
testRowDec = TestRow <$> D.int <*> D.double <*> D.utf8

bs1 :: ByteString
bs1 = mconcat
  [ "1,5.5,hello\n"
  , "2,-3.54,\n"
  , "3,42.0,\"Text in quotes\"\n"
  ]

bs2 :: ByteString
bs2 = hdr `mappend` bs1
  where
    hdr = "id, number, name\n"

csv1 :: Cassava.Csv
csv1 = V.fromList . fmap V.fromList $
  [ ["1","5.5","hello"]
  , ["2","-3.54", ""]
  , ["3", "42.0", "Text in quotes"]
  ]

decoded1 :: [TestRow]
decoded1 =
  [ TestRow 1 5.5 "hello"
  , TestRow 2 (-3.54) ""
  , TestRow 3 42 "Text in quotes"
  ]

parseTest :: Test
parseTest = TestLabel "parse" . TestCase $
  parseCassava Cassava.defaultDecodeOptions bs1 @?= pure csv1

decodeTest :: Test
decodeTest = TestLabel "decode" . TestCase $
  decodeFromCassava testRowDec csv1 @?= pure decoded1

parseDecodeTest :: Test
parseDecodeTest = TestLabel "parse+decode" . TestList $
  [ TestLabel "unheaded" . TestCase $
      parseDecodeFromCassava testRowDec Unheaded Cassava.defaultDecodeOptions bs1 @?=
        pure decoded1
  , TestLabel "headed" . TestCase $
      parseDecodeFromCassava testRowDec Headed Cassava.defaultDecodeOptions bs2 @?=
        pure decoded1
  ]

tests :: Test
tests = TestList [parseTest, decodeTest, parseDecodeTest]

main :: IO ()
main = do
  cs <- runTestTT tests
  when (errors cs > 0 || failures cs > 0)
    exitFailure
