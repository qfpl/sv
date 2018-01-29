{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.DecodeTest (test_Decode) where

import Control.Lens ((&), (.~))
import Data.ByteString
import Data.Functor.Alt
import Data.Semigroup
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Sv (SvConfig, defaultConfig, Headedness (Unheaded), headedness)
import Data.Sv.Decode

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

v3ios :: FieldDecode ByteString ByteString (V3 IntOrString)
v3ios = v3 intOrString

csv1 :: ByteString
csv1 = intercalate "\r\n" [
    "\"3\", \"4\", \"5\""
  , "\"quoted text\", unquoted text, 100"
  , "7, unquoted text, 5"
  ]

csv1' :: [V3 IntOrString]
csv1' =
  [ I <$> V3 3 4 5
  , V3 (S "quoted text") (S "unquoted text") (I 100)
  , V3 (I 7) (S "unquoted text") (I 5)
  ]

cfg :: SvConfig
cfg = defaultConfig & headedness .~ Unheaded

intOrStringTest :: TestTree
intOrStringTest =
    testCase "parse successfully" $
      parseDecode v3ios (Just cfg) csv1 @?= pure csv1'
