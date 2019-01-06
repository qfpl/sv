{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.EncodeTest (test_Encode) where

import Control.Lens (makeClassyPrisms)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Semigroup ((<>))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Sv
import qualified Data.Sv.Encode as E

data IntOrString =
  I Int | S String
  deriving (Eq, Ord, Show)

makeClassyPrisms ''IntOrString

data IntAndString = IAS { getInt :: Int, getString :: String }

test_Encode :: TestTree
test_Encode =
  testGroup "Encode" [
    divisibleTests
  , decidableTests
  , encodeTests
  , escapeTests
  , encodeNamedTests
  ]

intAndString :: Encode IntAndString
intAndString = contramap getInt E.int <> contramap getString E.string


opts :: EncodeOptions
opts = defaultEncodeOptions

intOrString :: Encode IntOrString
intOrString = E.encodeOf _I E.int <> E.encodeOf _S E.string

i :: IntOrString
i = I 5

s :: IntOrString
s = S "hello"

decidableTests :: TestTree
decidableTests =
  testGroup "decidable" [
    testCase "encode an Int" $
      encodeRow intOrString opts i @?= "5"
  , testCase "encode a String" $
      encodeRow intOrString opts s @?= "hello"
  ]

intEmptyAndString :: Encode IntAndString
intEmptyAndString = contramap getInt E.int <> E.empty <> contramap getString E.string

ias :: IntAndString
ias = IAS 10 "goodbye"

divisibleTests :: TestTree
divisibleTests =
  testGroup "divisible" [
    testCase "encode an IntAndString" $
      encodeRow intAndString opts ias @?= "10,goodbye"
  , testCase "encode an IntAndString with an empty between" $
      encodeRow intEmptyAndString opts ias @?= "10,,goodbye"
  ]

encodeTests :: TestTree
encodeTests =
  testCase "multiple lines" $
    encode (divided intAndString intOrString) opts [(IAS 3 "book", I 4), (IAS 7 "film", S "ok")]
      @?= "3,book,4\n7,film,ok"

escapeTests :: TestTree
escapeTests =
  testGroup "escape" [
    testCase "string" $
      encodeRow E.string opts "hello \"test\" case" @?= "\"hello \"\"test\"\" case\""
  , testCase "text" $
      encodeRow E.text opts "this is also\"a test\"" @?= "\"this is also\"\"a test\"\"\""
  , testCase "bytestring - strict" $
      encodeRow E.byteString opts "\"test\"ing\" " @?= "\"\"\"test\"\"ing\"\" \""
  , testCase "bytestring - lazy" $
      encodeRow E.lazyByteString opts "\"" @?= "\"\"\"\""
  ]

data Three = Three {
  int :: Int
, double :: Double
, text :: Text
} deriving (Eq, Ord, Show)

three :: NameEncode Three
three =
  E.named "first" (contramap int E.int)
    <> E.named "\"Second\"" (contramap double E.doubleFast)
    <> E.named "third" (contramap text E.text)

myInt :: NameEncode Int
myInt = E.named "my int" E.int

encodeNamedTests :: TestTree
encodeNamedTests = testGroup "named" [
    testCase "empty decoder" $
      encodeNamed mempty opts [] @?= ""
  , testCase "single column, zero rows" $
      encodeNamed myInt opts [] @?= "my int"
  , testCase "single column, one row" $
      encodeNamed myInt opts [5] @?= "my int\n5"
  , testCase "single column, many rows" $
      encodeNamed myInt opts [1..5] @?= "my int\n1\n2\n3\n4\n5"
  , testCase "multiple columns, zero rows" $
      encodeNamed three opts [] @?= "first,\"\"\"Second\"\"\",third"
  , testCase "multiple columns, one row" $
      encodeNamed three opts [Three 1 2 "th\"ree"]
        @?= "first,\"\"\"Second\"\"\",third\n1,2,\"th\"\"ree\""
  , testCase "multiple columns, multiple rows" $
      encodeNamed three opts [Three 1 2 "three", Three 4 5 "SIX"]
        @?= "first,\"\"\"Second\"\"\",third\n1,2,three\n4,5,SIX"
  ]
