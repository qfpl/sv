{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.DecodeTest (test_Decode) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Data.ByteString
import Data.Functor.Alt
import Data.Semigroup
import Text.Trifecta (Result(Success, Failure))

import Data.Csv.Csv (Headedness (Unheaded))
import Data.Csv.Decode

test_Decode :: TestTree
test_Decode =
  testGroup "Decode" [
    intOrStringTest
  ]

data IntOrString =
  I Int | S String
  deriving (Eq, Ord, Show)

intOrString :: FieldDecode ByteString ByteString IntOrString
intOrString = I <$> int <!> S <$> string

data V3 a =
  V3 a a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

v3 :: Semigroup e => FieldDecode e s a -> FieldDecode e s (V3 a)
v3 f = sequenceA (V3 f f f)

v3ios :: RowDecode ByteString ByteString (V3 IntOrString)
v3ios = row (v3 intOrString)

csv1 :: ByteString
csv1 = intercalate "\r\n" [
    "\"3\", \"4\", \"5\""
  , "\"quoted text\", unquoted text, 100"
  , "7, unquoted text, 5"
  ]

csv1' :: [V3 IntOrString]
csv1' =
  [ I <$> V3 3 4 5
  , V3 (S "quoted text") (S " unquoted text") (I 100)
  , V3 (I 7) (S " unquoted text") (I 5)
  ]

intOrStringTest :: TestTree
intOrStringTest =
    testCase "parse successfully" $
      case parseDecode v3ios Unheaded csv1 of
        Failure _ -> assertFailure "Parse failed"
        Success z -> z @?= pure csv1'

