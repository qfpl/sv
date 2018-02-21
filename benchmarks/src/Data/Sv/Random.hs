{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.Random where

import Control.Lens (makeLenses, makePrisms, traverseOf)
import Control.Monad ((>=>), replicateM)
import Data.Semigroup (Semigroup ((<>)))
import Data.ByteString
import Data.Functor.Contravariant
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
import Text.Escape (Unescaped (Unescaped, getRawUnescaped))
import Text.Newline (Newline (CR, LF, CRLF))
import Text.Quote (Quote (SingleQuote, DoubleQuote))

data Product =
  Product { i :: Int, f :: Float, d :: Double }
  deriving Show

data Coproduct
  = I Int
  | B ByteString
  | D Double
  deriving Show

type Row = Either LongRow ShortRow

data LongRow = LongRow {
    _lrB :: ByteString
  , _lrS :: String
  , _lrI :: Int
  , _lrG :: Integer
  , _lrF :: Float
  , _lrD :: Double
  , _lrP :: Product
  , _lrC :: Coproduct 
  } deriving Show

data ShortRow =
  ShortRow { _1 :: ByteString, _2 ::ByteString, _3 :: ByteString }
  deriving Show

makePrisms ''Coproduct

makeLenses ''LongRow

utf8Gen :: Gen ByteString
utf8Gen = Gen.utf8 (Range.constantFrom 10 0 100) Gen.alphaNum

numRange :: Num a => Range.Range a
numRange = Range.constantFrom 0 (-100000) 10000

productGen :: Gen Product
productGen = Product <$> Gen.int numRange <*> Gen.float numRange <*> Gen.double numRange

coproductGen :: Gen Coproduct
coproductGen =
  Gen.choice [
    I <$> Gen.int numRange
  , B <$> utf8Gen
  , D <$> Gen.double numRange
  ]

longRowGen :: Gen LongRow
longRowGen =
  LongRow
    <$> utf8Gen
    <*> Gen.string (Range.constantFrom 10 0 100) Gen.alphaNum
    <*> Gen.int numRange
    <*> Gen.integral numRange
    <*> Gen.float numRange
    <*> Gen.double numRange
    <*> productGen
    <*> coproductGen

shortRowGen :: Gen ShortRow
shortRowGen =
  ShortRow <$> utf8Gen <*> utf8Gen <*> utf8Gen

rowGen :: Gen Row
rowGen =
  Gen.frequency [
    (9, Left <$> longRowGen)
  , (1, Right <$> shortRowGen)
  ]

rowEnc :: Encode Row
rowEnc = E.chosen longRowEnc shortRowEnc

shortRowEnc :: Encode ShortRow
shortRowEnc =
  contramap _1 E.byteString <> contramap _2 E.byteString <> contramap _3 E.byteString

productEnc :: Encode Product
productEnc =
  contramap i E.int <> contramap f E.float <> contramap d E.double

coproductEnc :: Encode Coproduct
coproductEnc =
  E.encodeOf _I  E.int <> E.encodeOf _B E.byteString <> E.encodeOf _D E.double

longRowEnc :: Encode LongRow
longRowEnc = mconcat [
    E.encodeOf lrB E.byteString
  , E.encodeOf lrS E.string
  , E.encodeOf lrI E.int
  , E.encodeOf lrG E.integer
  , E.encodeOf lrF E.float
  , E.encodeOf lrD E.double
  , E.encodeOf lrP productEnc
  , E.encodeOf lrC coproductEnc
  ]

rowDec :: FieldDecode' ByteString Row
rowDec = D.either longRowDec shortRowDec

shortRowDec :: FieldDecode' ByteString ShortRow
shortRowDec = ShortRow <$> D.byteString <*> D.byteString <*> D.byteString

productDec :: FieldDecode' ByteString Product
productDec = Product <$> D.int <*> D.float <*> D.double

coproductDec :: FieldDecode' ByteString Coproduct
coproductDec = I <$> D.int <!> B <$> D.byteString <!> D <$> D.double

longRowDec :: FieldDecode' ByteString LongRow
longRowDec = LongRow <$> D.byteString <*> D.string <*> D.int <*> D.integer <*> D.float <*> D.double <*> productDec <*> coproductDec

samples :: Gen a -> Int -> IO [a]
samples = flip replicateM . Gen.sample

rows :: Int -> IO [Row]
rows = samples rowGen

rowsSv :: Int -> IO (Sv ByteString)
rowsSv = fmap (E.encodeSv defaultEncodeOptions rowEnc Nothing) . rows

rowsSv' :: Int -> IO (Sv ByteString)
rowsSv' =
  let randomNewline :: Newline -> IO Newline
      randomNewline = const (Gen.sample (Gen.element [LF, CR, CRLF]))
      randomQuote :: IO (Maybe Quote)
      randomQuote = Gen.sample (Gen.element [Nothing, Just SingleQuote, Just DoubleQuote])
      requote :: Field ByteString -> Maybe Quote -> Field ByteString
      requote g m = case g of
        Unquoted a -> case m of
          Nothing -> Unquoted a
          Just x  -> Quoted x (Unescaped a)
        Quoted _ v -> case m of
          Nothing -> Unquoted (getRawUnescaped v)
          Just x  -> Quoted x v
      randomField :: Field ByteString -> IO (Field ByteString)
      randomField z = fmap (requote z) randomQuote
      randomNewlines :: Sv s -> IO (Sv s)
      randomNewlines = traverseOf traverseNewlines randomNewline
      randomQuotes :: Sv ByteString -> IO (Sv ByteString)
      randomQuotes = traverseOf (traverseRecords . fields) randomField
  in  rowsSv >=> randomNewlines >=> randomQuotes

randomSv :: Int -> IO ByteString
randomSv = fmap printSv . rowsSv'
