{-# LANGUAGE ScopedTypeVariables #-}

module Data.Sv.CassavaTest (test_CassavaAgreement) where

import qualified Data.ByteString as BS
import qualified Data.Csv as Csv
import Data.Sv (Validation (Failure, Success), Headedness (Unheaded), ParseOptions (_headedness), defaultParseOptions)
import qualified Data.Sv as Sv (parseDecode)
import Data.Sv.Decode (Decode')
import qualified Data.Sv.Decode as Sv
import Data.Vector as V
import Data.Tuple.Only (Only (Only, fromOnly))
import Hedgehog (Gen, (===), failure, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

test_CassavaAgreement :: TestTree
test_CassavaAgreement =
  testGroup "cassava agreement"
    [ cassavaAgreement "int" Sv.int (Gen.int (Range.linear (-10000000) 10000000))
    , cassavaAgreement "char" Sv.char (Gen.unicode)
    , cassavaAgreement "integer" Sv.integer (Gen.integral (Range.linear (-10000000) 10000000))
    , cassavaAgreement "string" Sv.string (Gen.string (Range.linear 1 500) Gen.unicode)
    , cassavaAgreement "bytestring" Sv.byteString (Gen.utf8 (Range.linear 1 500) Gen.unicode)
    , cassavaAgreement "float" Sv.float (Gen.float (Range.exponentialFloat (-10000000) 10000000))
    , cassavaAgreement "double" Sv.double (Gen.double (Range.exponentialFloat (-10000000) 10000000))
    ]

opts :: ParseOptions
opts = defaultParseOptions { _headedness = Unheaded }

-- | Test that decoding with sv gets the same result as decoding with cassava
cassavaAgreement :: forall a . (Csv.FromField a, Csv.ToField a, Show a, Eq a) => TestName -> Decode' BS.ByteString a -> Gen a -> TestTree
cassavaAgreement name dec gen = testProperty name $ property $ do
  a <- forAll gen
  let oa = Only a
  let sa = Csv.encode [oa]
  let cassava :: Either String [a]
      cassava = fmap fromOnly . V.toList <$> Csv.decode Csv.NoHeader sa
      sv = Sv.parseDecode dec opts sa
  case cassava of
    Left _ -> failure
    Right csa -> case sv of
      Failure _ -> failure
      Success sva -> do
        csa === sva
        sva === pure a
