module Data.Csv.Generators (
  genCsv
  , genCsvWithHeadedness
  , genNewline
  , genSep
  , genBetween
  , genEscaped
  , genQuote
  , genField
  , genRecord
  , genRecords
  , genPesarated1
  , genSeparated
  , genHeader
  , genCsvString
) where

import Control.Applicative ((<$>), liftA2, liftA3)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Separated      (Pesarated1 (Pesarated1), Separated (Separated), Separated1 (Separated1))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Csv.Csv        (Csv (Csv), Header (Header), Headedness, headedness, Separator)
import Data.Csv.Field      (Field (QuotedF, UnquotedF))
import Data.Csv.Record     (Record (Record), Records (Records))
import Text.Between        (Between (Between))
import Text.Escaped        (Escaped', escapeNel)
import Text.Newline        (Newline (CRLF, LF))
import Text.Space          (Spaces, Spaced)
import Text.Quote          (Quote (SingleQuote, DoubleQuote), Quoted (Quoted))

genCsv :: Gen Separator -> Gen Spaces -> Gen s -> Gen (Csv s)
genCsv sep spc s =
  let rs = genRecords spc s
      e  = Gen.list (Range.linear 0 5) genNewline
      h = Gen.maybe (genHeader spc s genNewline)
  in  Csv <$> sep <*> h <*> rs <*> e

genCsvWithHeadedness :: Gen Separator -> Gen Spaces -> Gen s -> Gen (Csv s, Headedness)
genCsvWithHeadedness sep spc s = fmap (\c -> (c, headedness c)) (genCsv sep spc s)

genNewline :: Gen Newline
genNewline =
  -- TODO put CR back in
  Gen.element [CRLF, LF]

genSep :: Gen Separator
genSep =
  Gen.element ['|', ',']

genBetween :: Gen Spaces -> Gen str -> Gen (Spaced str)
genBetween spc str =
  liftA3 Between spc str spc

genQuote :: Gen Quote
genQuote =
  Gen.element [SingleQuote, DoubleQuote]

genEscaped :: Gen a -> Gen (Escaped' a)
genEscaped a =
  escapeNel <$> Gen.nonEmpty (Range.linear 1 5) a

genQuoted :: Gen a -> Gen (Quoted a)
genQuoted =
  liftA2 Quoted genQuote . genEscaped

genField :: Gen Spaces -> Gen s -> Gen (Field s)
genField spc s =
  Gen.choice [
    UnquotedF <$> s
  , QuotedF <$> genBetween spc (genQuoted s)
  ]

genRecord :: Gen Spaces -> Gen s -> Gen (Record s)
genRecord spc s =
  Record <$> Gen.nonEmpty (Range.linear 1 10) (genField spc s)

genHeader :: Gen Spaces -> Gen s -> Gen Newline -> Gen (Header s)
genHeader spc s n =
  Header <$> genRecord spc s <*> n

genRecords :: Gen Spaces -> Gen s -> Gen (Records s)
genRecords spc s =
  Records <$> Gen.maybe (genPesarated1 genNewline (genRecord spc s))

genPesarated1 :: Gen a -> Gen b -> Gen (Pesarated1 a b)
genPesarated1 a b = Pesarated1 <$> genSeparated1 b a

genSeparated :: Gen a -> Gen b -> Gen (Separated a b)
genSeparated a b =
  Separated <$> Gen.list (Range.linear 0 1000) (liftA2 (,) a b)

genSeparated1 :: Gen a -> Gen b -> Gen (Separated1 a b)
genSeparated1 a b = Separated1 <$> a <*> genSeparated b a

genCsvString :: Gen String
genCsvString =
  let genNewlineString = Gen.element ["\n", "\r", "\r\n"]
      genCsvRowString :: Gen String
      genCsvRowString = intercalate "," <$> Gen.list (Range.linear 1 100) genCsvField
      enquote c s = fmap (\z -> c <> z <> c) s
      genCsvFieldString :: Gen String
      genCsvFieldString = Gen.list (Range.linear 0 100) Gen.alphaNum
      genCsvField =
        Gen.choice [
          enquote "\"" genCsvFieldString
        , enquote "'" genCsvFieldString
        , genCsvFieldString
        ]
  in  intercalate <$> genNewlineString <*> Gen.list (Range.linear 0 1000) genCsvRowString
