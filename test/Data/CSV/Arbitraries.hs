module Data.CSV.Arbitraries where

import Control.Applicative ((<$>), liftA2, liftA3)
import Data.Separated      (Pesarated1 (Pesarated1), Separated (Separated), Separated1 (Separated1))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.CSV.CSV        (CSV (CSV), Records (Records))
import Data.CSV.Field      (Field (QuotedF, UnquotedF))
import Data.CSV.Record     (Record (Record))
import Text.Between        (Between (Between))
import Text.Newline        (Newline (CR, CRLF, LF))
import Text.Quote          (Escaped (SeparatedByEscapes), Quote (SingleQuote, DoubleQuote), Quoted (Quoted))

genCsv :: Gen Char -> Gen spc -> Gen str -> Gen (CSV spc str)
genCsv sep spc str =
  let ns = Gen.maybe genNewline
      rs = genRecords spc str
  in  liftA3 CSV sep rs ns

genNewline :: Gen Newline
genNewline =
  -- TODO put CR back in
  Gen.element [CRLF, LF]

genSep :: Gen Char
genSep =
  Gen.element ['|', ',']

genBetween :: Gen spc -> Gen str -> Gen (Between spc str)
genBetween spc str =
  liftA3 Between spc str spc

genQuote :: Gen Quote
genQuote =
  Gen.element [SingleQuote, DoubleQuote]

genEscaped :: Gen a -> Gen (Escaped a)
genEscaped a =
  SeparatedByEscapes <$> Gen.nonEmpty (Range.linear 1 5) a

genQuoted :: Gen a -> Gen (Quoted a)
genQuoted =
  liftA2 Quoted genQuote . genEscaped

genField :: Gen spc -> Gen str -> Gen (Field spc str)
genField spc str =
  Gen.choice [
    UnquotedF <$> str
  , QuotedF <$> genBetween spc (genQuoted str)
  ]

genRecord :: Gen spc -> Gen str -> Gen (Record spc str)
genRecord spc str =
  Record <$> Gen.nonEmpty (Range.linear 1 50) (genField spc str)

genRecords :: Gen spc -> Gen str -> Gen (Records spc str)
genRecords spc str =
  Records <$> genPesarated1 spc str

genPesarated1 :: Gen spc -> Gen str -> Gen (Pesarated1 Newline (Record spc str))
genPesarated1 spc str =
  let r = genRecord spc str 
  in  Pesarated1 <$> (Separated1 <$> r <*> genSeparated genNewline r)

genSeparated :: Gen a -> Gen b -> Gen (Separated a b)
genSeparated a b =
  Separated <$> Gen.list (Range.linear 0 10000) (liftA2 (,) a b)

