{-# LANGUAGE ScopedTypeVariables #-}

module Data.Sv.CassavaTest (test_CassavaAgreement) where

import qualified Data.ByteString as BS
import qualified Data.Csv as Csv
import Data.Sv (Validation (Failure, Success), Headedness (Unheaded), ParseOptions (_headedness), defaultParseOptions, defaultEncodeOptions)
import qualified Data.Sv as Sv (parseDecode, encode)
import Data.Sv.Decode (Decode')
import qualified Data.Sv.Decode as Sv
import Data.Sv.Encode (Encode)
import qualified Data.Sv.Encode as E
import qualified Data.Vector as V
import Data.Tuple.Only (Only (fromOnly))
import Hedgehog (Gen, TestLimit, (===), failure, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

test_CassavaAgreement :: TestTree
test_CassavaAgreement =
  testGroup "cassava agreement"
    [ cassavaAgreement "int" Sv.int E.int (Gen.int (Range.linear (-10000000) 10000000))
    , cassavaAgreement "char" Sv.char E.char (Gen.unicode)
    , cassavaAgreement "integer" Sv.integer E.integer (Gen.integral (Range.linear (-10000000) 10000000))
    , cassavaAgreement "string" Sv.string E.string (Gen.string (Range.linear 1 500) Gen.unicode)
    , cassavaAgreement' "bytestring" 5000 Sv.byteString E.byteString bsGen
    , cassavaAgreement' "float" 5000 Sv.float E.float (Gen.float (Range.exponentialFloat (-1000000000) 1000000000))
    , cassavaAgreement' "double" 5000 Sv.double E.double (Gen.double (Range.exponentialFloat (-1000000000) 1000000000))
    ]

opts :: ParseOptions
opts = defaultParseOptions { _headedness = Unheaded }

-- | Test that decoding with sv gets the same result as decoding with cassava
cassavaAgreement :: forall a . (Csv.FromField a, Csv.ToField a, Show a, Eq a) => TestName -> Decode' BS.ByteString a -> Encode a -> Gen a -> TestTree
cassavaAgreement t = cassavaAgreement' t 100

cassavaAgreement' :: forall a . (Csv.FromField a, Csv.ToField a, Show a, Eq a) => TestName -> TestLimit -> Decode' BS.ByteString a -> Encode a -> Gen a -> TestTree
cassavaAgreement' name reps dec enc gen = testProperty name $ withTests reps $ property $ do
  a <- forAll gen
  let sa = Sv.encode enc defaultEncodeOptions [a]
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

-- we filter out anything that is all spaces, since cassava and sv
-- behave differently in that case and I'll accept that.
bsGen :: Gen BS.ByteString
bsGen = Gen.filter p $ Gen.utf8 (Range.linear 1 600) Gen.unicode
  where
    p x = BS.null x || BS.any (not.isSpace) x
    isSpace = (<= 32)
