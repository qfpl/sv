{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.DecodeTest (test_Decode) where

import Control.Applicative (liftA2)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

--import Data.Sv
import qualified Data.Sv.Decode as D

test_Decode :: TestTree
test_Decode =
  testGroup "Decode" [
    intOrStringTest
  , varyingLengthTest
  ]

data IntOrString =
  I Int | S String
  deriving (Eq, Ord, Show)

intOrString :: D.Decode' ByteString IntOrString
intOrString = I <$> D.int <!> S <$> D.string

data V3 a =
  V3 a a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

v3 :: Semigroup e => D.Decode e s a -> D.Decode e s (V3 a)
v3 f = sequenceA (V3 f f f)

v3ios :: D.Decode ByteString ByteString (V3 IntOrString)
v3ios = v3 intOrString

csv1 :: LBS.ByteString
csv1 = LBS.intercalate "\n" [
    "\"3\",\"4\",\"5\""
  , "\"quoted text\",unquoted text,100"
  , "7,unquoted text,5"
  ] <> "\n"

csv1' :: [V3 IntOrString]
csv1' =
  [ I <$> V3 3 4 5
  , V3 (S "quoted text") (S "unquoted text") (I 100)
  , V3 (I 7) (S "unquoted text") (I 5)
  ]

opts :: D.ParseOptions
opts = D.ParseOptions ',' D.Unheaded

intOrStringTest :: TestTree
intOrStringTest =
    testCase "parse successfully" $
      D.parseDecode v3ios opts csv1 @?= pure csv1'

varyingLength :: LBS.ByteString
varyingLength = LBS.intercalate "\r\n" [
    "one"
  , "one,two"
  , "one,two,three"
  , "one,two,three,four"
  , "one,two,three,four,five"
  ] <> "\n"

str2 :: D.Decode' ByteString (ByteString, ByteString)
str2 = liftA2 (,) D.contents D.contents

varyingLengthTest :: TestTree
varyingLengthTest =
  testCase "varyingLength has all the right errors" $
    D.parseDecode str2 opts varyingLength @?=
      D.Failure (D.DecodeErrors (D.UnexpectedEndOfRow :| [
        D.ExpectedEndOfRow (V.fromList ["three"])
      , D.ExpectedEndOfRow (V.fromList ["three", "four"])
      , D.ExpectedEndOfRow (V.fromList ["three", "four", "five"])
      ]))
