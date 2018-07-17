{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Sv.Laws (test_Laws) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroupoid (Semigroupoid (o))
import qualified Data.Validation as V
import Data.Vector (Vector, fromList, toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), CoArbitrary (coarbitrary), Gen, testProperty, oneof, listOf)

import Data.Sv
import qualified Data.Sv.Decode as D

test_Laws :: TestTree
test_Laws =
  testGroup "Laws"
    [ testGroup "Decode Semigroupoid" $ fmap (uncurry testProperty)
        [ ("Decode category associativity", decodeAssoc)
        ]
    ]

decodeAssoc :: Gen Bool
decodeAssoc = do
  dec1 <- decodeGen :: Gen (Decode ByteString ByteString ByteString)
  dec2 <- decodeGen :: Gen (Decode ByteString ByteString ByteString)
  dec3 <- decodeGen :: Gen (Decode ByteString ByteString ByteString)
  v <- arbOut
  i <- arbOut
  let comp1 = (dec1 `o` dec2) `o` dec3
  let comp2 = dec1 `o` (dec2 `o` dec3)
  pure (eqDecode v i comp1 comp2)


eqDecode :: (Eq e, Eq a) => Vector s -> Ind -> Decode e s a -> Decode e s a -> Bool
eqDecode v i d e =
  D.runDecode d v i == D.runDecode e v i

decodeGen :: (CoArbitrary (In s), Arbitrary (Out a), Arbitrary (Out e)) => Gen (Decode e s a)
decodeGen =
  buildDecode <$> (unwrap <$> arbitrary)

unwrap :: (Input s -> Output e a) -> Vector s -> Ind -> (Validation e a, Ind)
unwrap = curry . dimap Input unwrapO

newtype Output e a =
  Output { unwrapO :: (Validation e a, Ind) }

newtype Input s = Input (Vector s, Ind)

newtype In a = In a
newtype Out a = Out { unout :: a }

coarbIn :: CoArbitrary (In a) => a -> Gen b -> Gen b
coarbIn = coarbitrary . In

arbOut :: Arbitrary (Out a) => Gen a
arbOut = unout <$> arbitrary

instance (Arbitrary (Out e), Arbitrary (Out a)) => Arbitrary (Output e a) where
  arbitrary =
    Output <$>
      ((,) <$> oneof [V.Failure <$> arbOut, V.Success <$> arbOut]
          <*> (Ind <$> arbOut))

instance (CoArbitrary (In (Vector s)), CoArbitrary (In Ind)) => CoArbitrary (Input s) where
  coarbitrary (Input (v,i)) = coarbIn v . coarbIn i

instance Arbitrary (Out ByteString) where
  arbitrary = Out <$> (BS.pack <$> arbitrary)

instance CoArbitrary (In ByteString) where
  coarbitrary (In bs) = coarbitrary (BS.unpack bs)

instance Arbitrary (Out Ind) where
  arbitrary = Out . Ind <$> arbitrary

instance Arbitrary (Out Int) where
  arbitrary = Out <$> arbitrary

instance CoArbitrary (In s) => CoArbitrary (In (Vector s)) where
  coarbitrary (In v) = coarbitrary (In <$> toList v)

instance CoArbitrary (In Ind) where
  coarbitrary (In (Ind i)) = coarbitrary i

instance Arbitrary (Out e) => Arbitrary (Out (DecodeErrors e)) where
  arbitrary = Out . DecodeErrors <$> arbOut

instance Arbitrary (Out e) => Arbitrary (Out (NonEmpty e)) where
  arbitrary = Out <$> ((:|) <$> arbOut <*> arbOut)

instance Arbitrary (Out e) => Arbitrary (Out [e]) where
  arbitrary = Out <$> listOf arbOut

instance Arbitrary (Out e) => Arbitrary (Out (Vector e)) where
  arbitrary = Out . fromList <$> arbOut

instance Arbitrary (Out e) => Arbitrary (Out (DecodeError e)) where
  arbitrary = Out <$> oneof
    [ pure UnexpectedEndOfRow
    , ExpectedEndOfRow <$> arbOut
    , UnknownCategoricalValue <$> arbOut <*> arbOut
    , BadParse <$> arbOut
    , BadDecode <$> arbOut
    ]
