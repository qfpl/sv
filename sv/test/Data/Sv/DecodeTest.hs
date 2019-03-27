{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sv.DecodeTest (test_Decode) where

import Control.Applicative (liftA2)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Alt ((<!>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Text.Read (readMaybe)
import Data.Semigroup (Semigroup ((<>)))
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
  , namedTest
  , displayErrorsTest
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
        Failure (DecodeErrors (pure (BadDecode "Couldn't decode \"false\" as a double")))
  ]

-- This CSV has enough columns to make an Item, it has more columns than a
-- SemiItem/SemiItem2, and not enough columns for a SuperItem/SuperItem2
namedCsv :: LBS.ByteString
namedCsv =
  LBS.intercalate "\n" [
    "id,name,cost,units"
  , "1,fridge,600,5"
  , "2,stereo,1499.99,2"
  , "3,dryer,748.95,3"
  ]

data Item = Item Int Text Double Int deriving (Eq, Show)
data SemiItem = SemiItem Text Double deriving (Eq, Show)
data SemiItem2 = SemiItem2 Int Double deriving (Eq, Show)
data SuperItem = SuperItem Int Text Text Double Int deriving (Eq, Show)
data SuperItem2 = SuperItem2 Int Text Text Double Int Double deriving (Eq, Show)

inOrder :: NameDecode' ByteString Item
inOrder =
  Item <$> D.column "id" D.int <*> D.column "name" D.utf8
    <*> D.column "cost" D.double <*> D.column "units" D.int

outOrder :: NameDecode' ByteString Item
outOrder =
  (\n u c i -> Item i n c u) <$> D.column "name" D.utf8
    <*> D.column "units" D.int <*> D.column "cost" D.double
    <*> D.column "id" D.int

inOrderSemi :: NameDecode' ByteString SemiItem
inOrderSemi =
  SemiItem <$> D.column "name" D.utf8 <*> D.column "cost" D.double

outOrderSemi :: NameDecode' ByteString SemiItem2
outOrderSemi =
  SemiItem2 <$> D.column "units" D.int <*> D.column "cost" D.double

super :: NameDecode' ByteString SuperItem
super =
  SuperItem <$> D.column "id" D.int <*> D.column "name" D.utf8
    <*> D.column "manufacturer" D.utf8
    <*> D.column "cost" D.double <*> D.column "units" D.int

super2 :: NameDecode' ByteString SuperItem2
super2 =
  SuperItem2 <$> D.column "id" D.int <*> D.column "name" D.utf8
    <*> D.column "manufacturer" D.utf8
    <*> D.column "cost" D.double <*> D.column "units" D.int
    <*> D.column "profit" D.double

namedTest :: TestTree
namedTest = testGroup "Named decodes"
  [ testCase "columns in order" $
      parseDecodeNamed inOrder defaultParseOptions namedCsv @?=
        pure [Item 1 "fridge" 600 5, Item 2 "stereo" 1499.99 2, Item 3 "dryer" 748.95 3]
  , testCase "columns out of order" $
      parseDecodeNamed outOrder defaultParseOptions namedCsv @?=
        pure [Item 1 "fridge" 600 5, Item 2 "stereo" 1499.99 2, Item 3 "dryer" 748.95 3]
  , testCase "columns in order, with some skipped" $
      parseDecodeNamed inOrderSemi defaultParseOptions namedCsv @?=
        pure [SemiItem "fridge" 600, SemiItem "stereo" 1499.99, SemiItem "dryer" 748.95]
  , testCase "columns out of order, with some skipped" $
      parseDecodeNamed outOrderSemi defaultParseOptions namedCsv @?=
        pure [SemiItem2 5 600, SemiItem2 2 1499.99, SemiItem2 3 748.95]
  , testCase "one missing column" $
      parseDecodeNamed super defaultParseOptions namedCsv @?=
        Failure (DecodeErrors (MissingColumn "manufacturer":|[]))
  , testCase "multiple missing columns" $
      parseDecodeNamed super2 defaultParseOptions namedCsv @?=
        Failure (DecodeErrors (MissingColumn "manufacturer":|[MissingColumn "profit"]))
  , testCase "empty document (missing header)" $
      parseDecodeNamed inOrder defaultParseOptions "" @?=
        Failure (DecodeErrors (MissingHeader:|[]))
  , testCase "misconfigured" $
      parseDecodeNamed inOrder opts namedCsv @?=
        Failure badConfigErrors
  ]

badConfigErrors :: DecodeErrors ByteString
badConfigErrors =
  DecodeErrors (
    BadConfig "Your ParseOptions indicates a CSV with no header (Unheaded), but your decoder requires column names."
      :|[]
  )

displayErrorsTest :: TestTree
displayErrorsTest = testGroup "displayErrors" [
    testCase "single error / BadParse" $
      displayErrors (DecodeErrors (BadParse "missing comma":|[])) @?=
        "The following error occurred:\n" <>
        "  Parsing the document failed. The error was: missing comma"
  , testCase "two errors / UnexpectedEndOfRow, BadDecode" $
      displayErrors (DecodeErrors (UnexpectedEndOfRow:|[BadDecode "Could not decode \"3.z\" as a Double"])) @?=
        "The following 2 errors occurred:\n" <>
        "  Expected more fields, but the row ended.\n" <>
        "  Decoding a field failed: Could not decode \"3.z\" as a Double"
  , testCase "BadConfig" $
      displayErrors badConfigErrors @?=
        "The following error occurred:\n" <>
        "  sv was misconfigured: Your ParseOptions indicates a CSV with no header (Unheaded), but your decoder requires column names."
  , testCase "Missing Columns" $
      displayErrors (DecodeErrors (MissingColumn "name":|[MissingColumn "age", MissingColumn "address"])) @?=
        "The following 3 errors occurred:\n" <>
        "  Could not find required column \"name\"\n" <>
        "  Could not find required column \"age\"\n" <>
        "  Could not find required column \"address\""
  , testCase "MissingHeader" $
      displayErrors (DecodeErrors (MissingHeader:|[])) @?=
        "The following error occurred:\n" <>
        "  A header row was required, but one was not found."
  , testCase "ExpectedEndOfRow" $
      displayErrors (DecodeErrors (ExpectedEndOfRow ["hello", "yes", "no"]:|[])) @?=
        "The following error occurred:\n" <>
        "  Expected fewer fields in the row. The extra fields contained: \"hello\", \"yes\", \"no\""
  , testCase "UnknownCategoricalValue" $
      displayErrors (DecodeErrors (UnknownCategoricalValue "yes" [["y","true","ok"],["n","false","nope"]] :|[])) @?=
        "The following error occurred:\n" <>
        "  Unknown categorical value found: \"yes\". " <>
          "Expected one of: \"y\", \"true\", \"ok\", \"n\", \"false\", \"nope\""
  ]
