{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.EncodeTest (test_Encode) where

import Control.Lens (makeClassyPrisms)
import Data.Functor.Contravariant
import Data.Semigroup ((<>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Sv.Encode

data IntOrString =
  I Int | S String
  deriving (Eq, Ord, Show)

makeClassyPrisms ''IntOrString

data IntAndString = IAS { getInt :: Int, getString :: String }

test_Encode :: TestTree
test_Encode =
  testGroup "Encode" [
    intOrStringTests
  , intAndStringTests
  ]

opts :: EncodeOptions
opts = defaultEncodeOptions

intOrString :: Encode IntOrString
intOrString = fromFold _I int <> fromFold _S string

i :: IntOrString
i = I 5

s :: IntOrString
s = S "hello"

intOrStringTests :: TestTree
intOrStringTests =
  testGroup "ios" [
    testCase "encode an Int" $
      encode opts intOrString i @?= "\"5\""
  , testCase "encode a String" $
      encode opts intOrString s @?= "\"hello\""
  ]

intAndString :: Encode IntAndString
intAndString = contramap getInt showEncode <> contramap getString string

ias :: IntAndString
ias = IAS 10 "goodbye"

intAndStringTests :: TestTree
intAndStringTests =
  testGroup "ias" [
    testCase "encode an IntAndString" $
      encode opts intAndString ias @?= "\"10\",\"goodbye\""
  ]
