{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Sv.Random where

import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, makePrisms, traverseOf)
import Control.Monad ((>=>), replicateM)
import Data.Csv (FromRecord (..), FromField (..), (.!))
import Data.Semigroup (Semigroup ((<>)))
import Data.ByteString (ByteString)
import Data.Functor.Contravariant
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Sv.Encode as E
import Text.Escape (Unescaped (Unescaped, getRawUnescaped))
import Text.Newline (Newline (LF, CRLF))
import Text.Quote (Quote (SingleQuote, DoubleQuote))

data Product =
  Product { i :: Int, f :: Float, d :: Double }
  deriving (Show, Generic)

instance NFData Product

data Coproduct
  = I Int
  | B ByteString
  | D Double
  deriving (Show, Generic)

instance NFData Coproduct

instance FromField Coproduct where
  parseField a = I <$> parseField a <|> B <$> parseField a <|> D <$> parseField a

data Row = Long LongRow | Short ShortRow deriving (Show, Generic)

instance NFData Row

data LongRow = LongRow {
    _lrB :: ByteString
  , _lrS :: String
  , _lrI :: Int
  , _lrG :: Integer
  , _lrF :: Float
  , _lrD :: Double
  , _lrP :: Product
  , _lrC :: Coproduct 
  } deriving (Show, Generic)

instance NFData LongRow

instance FromRecord LongRow where
  parseRecord v = case V.length v of
    10 -> LongRow <$> v.!0 <*> v.!1 <*> v.!2 <*> v.!3 <*> v.!4 <*> v.!5 <*> (Product <$> v.!6 <*> v.!7 <*> v.!8) <*> v.!9
    _  -> empty

data ShortRow =
  ShortRow { _1 :: ByteString, _2 ::ByteString, _3 :: ByteString }
  deriving (Show, Generic)

instance NFData ShortRow

instance FromRecord ShortRow where
  parseRecord v = case V.length v of
    3  -> ShortRow <$> v.!0 <*> v.!1 <*> v.!2
    _  -> empty

instance FromRecord Row where
  parseRecord v = Long <$> parseRecord v <|> Short <$> parseRecord v 

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
    (9, Long <$> longRowGen)
  , (1, Short <$> shortRowGen)
  ]

rowEnc :: Encode Row
rowEnc = E.choose (\x -> case x of {Long r -> Left r ; Short r -> Right r}) longRowEnc shortRowEnc

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

rowDec :: Decode' ByteString Row
rowDec =  Long <$> longRowDec <!> Short <$> shortRowDec

shortRowDec :: Decode' ByteString ShortRow
shortRowDec = ShortRow <$> D.byteString <*> D.byteString <*> D.byteString

productDec :: Decode' ByteString Product
productDec = Product <$> D.int <*> D.float <*> D.double

coproductDec :: Decode' ByteString Coproduct
coproductDec = I <$> D.int <!> B <$> D.byteString <!> D <$> D.double

longRowDec :: Decode' ByteString LongRow
longRowDec = LongRow <$> D.byteString <*> D.string <*> D.int <*> D.integer <*> D.float <*> D.double <*> productDec <*> coproductDec

samples :: Gen a -> Int -> IO [a]
samples = flip replicateM . Gen.sample

rows :: Int -> IO [Row]
rows = samples rowGen

rowsSv :: Int -> IO (Sv ByteString)
rowsSv = fmap (E.encodeSv rowEnc defaultEncodeOptions Nothing) . rows

rowsSv' :: Int -> IO (Sv ByteString)
rowsSv' =
  let randomNewline :: Newline -> IO Newline
      randomNewline = const (Gen.sample (Gen.element [LF, CRLF]))
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
