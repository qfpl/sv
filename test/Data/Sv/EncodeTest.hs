{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.EncodeTest (test_Encode) where

import Control.Lens (makeClassyPrisms)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Semigroup ((<>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Sv.Encode

data IntOrString =
  I Int | S String
  deriving (Eq, Ord, Show)

makeClassyPrisms ''IntOrString

data IntAndString = IAS { getInt :: Int, getString :: String }

intAndString :: Encode IntAndString
intAndString = contramap getInt int <> contramap getString string

test_Encode :: TestTree
test_Encode =
  testGroup "Encode" [
    divisibleTests
  , decidableTests
  , encodeTests
  , escapeTests
  ]

opts :: EncodeOptions
opts = defaultEncodeOptions

intOrString :: Encode IntOrString
intOrString = fromFold _I int <> fromFold _S string

i :: IntOrString
i = I 5

s :: IntOrString
s = S "hello"

decidableTests :: TestTree
decidableTests =
  testGroup "decidable" [
    testCase "encode an Int" $
      encodeRow opts intOrString i @?= "\"5\""
  , testCase "encode a String" $
      encodeRow opts intOrString s @?= "\"hello\""
  ]

intEmptyAndString :: Encode IntAndString
intEmptyAndString = contramap getInt int <> empty <> contramap getString string

ias :: IntAndString
ias = IAS 10 "goodbye"

divisibleTests :: TestTree
divisibleTests =
  testGroup "divisible" [
    testCase "encode an IntAndString" $
      encodeRow opts intAndString ias @?= "\"10\",\"goodbye\""
  , testCase "encode an IntAndString with an empty between" $
      encodeRow opts intEmptyAndString ias @?= "\"10\",\"\",\"goodbye\""
  ]

encodeTests :: TestTree
encodeTests =
  testCase "multiple lines" $
    encode opts (divided intAndString intOrString) [(IAS 3 "book", I 4), (IAS 7 "film", S "ok")]
      @?= "\"3\",\"book\",\"4\"\r\n\"7\",\"film\",\"ok\""

escapeTests :: TestTree
escapeTests =
  testGroup "escape" [
    testCase "string" $
      encodeRow opts string "hello \"test\" case" @?= "\"hello \"\"test\"\" case\""
  , testCase "text" $
      encodeRow opts text "this is also\"a test\"" @?= "\"this is also\"\"a test\"\"\""
  , testCase "bytestring - strict" $
      encodeRow opts byteString "\"test\"ing\" " @?= "\"\"\"test\"\"ing\"\" \""
  , testCase "bytestring - lazy" $
      encodeRow opts lazyByteString "\"" @?= "\"\"\"\""
  ]
