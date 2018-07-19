{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.DecodeTest (test_Decode) where

import Control.Applicative (liftA2)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Text.Read (readMaybe)
import Data.Semigroup (Semigroup)
import Data.Semigroupoid (Semigroupoid (o))
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text

import Data.Sv
import qualified Data.Sv.Decode as D

test_Decode :: TestTree
test_Decode =
  testGroup "Decode" [
    intOrStringTest
  , varyingLengthTest
  , semigroupoidTest
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
  ]

csv1' :: [V3 IntOrString]
csv1' =
  [ I <$> V3 3 4 5
  , V3 (S "quoted text") (S "unquoted text") (I 100)
  , V3 (I 7) (S "unquoted text") (I 5)
  ]

opts :: ParseOptions
opts = ParseOptions comma Unheaded

intOrStringTest :: TestTree
intOrStringTest =
    testCase "parse successfully" $
      parseDecode v3ios opts csv1 @?= pure csv1'

varyingLength :: LBS.ByteString
varyingLength = LBS.intercalate "\r\n" [
    "one"
  , "one,two"
  , "one,two,three"
  , "one,two,three,four"
  , "one,two,three,four,five"
  ]

str2 :: D.Decode' ByteString (ByteString, ByteString)
str2 = liftA2 (,) D.contents D.contents

varyingLengthTest :: TestTree
varyingLengthTest =
  testCase "varyingLength has all the right errors" $
    parseDecode str2 opts varyingLength @?=
      Failure (DecodeErrors (UnexpectedEndOfRow :| [
        ExpectedEndOfRow (V.fromList ["three"])
      , ExpectedEndOfRow (V.fromList ["three", "four"])
      , ExpectedEndOfRow (V.fromList ["three", "four", "five"])
      ]))

semiTestString1 :: LBS.ByteString
semiTestString1 = "hello,5,6.6,goodbye"

semiTestString2 :: LBS.ByteString
semiTestString2 = "hello,no,6.6,goodbye"

semiTestString3 :: LBS.ByteString
semiTestString3 = "hello,5,false,goodbye"

parseDecoder :: D.Decode' ByteString Int
parseDecoder = D.contents D.>>==
  \bs -> validateMaybe (D.BadDecode bs) . readMaybe . BS8.unpack $ bs

data Semi = Semi Text Int Double Text deriving (Eq, Show)

semiD :: D.Decode' ByteString Semi
semiD = Semi <$> D.utf8 <*> (parseDecoder `o` D.contents) <*> D.double <*> D.utf8

semigroupoidTest :: TestTree
semigroupoidTest = testGroup "Semigroupoid Decode"
  [ testCase "Does the right thing in the case of success" $
      parseDecode semiD opts semiTestString1 @?=
        pure [Semi "hello" 5 6.6 "goodbye"]
  , testCase "Does the right thing in the case of left failure" $
      parseDecode semiD opts semiTestString2 @?=
        Failure (DecodeErrors (pure (BadDecode "no")))
  , testCase "Does the right thing in the case of right failure" $
      parseDecode semiD opts semiTestString3 @?=
        Failure (DecodeErrors (pure (BadDecode "Couldn't parse \"false\" as a double")))
  ]
