module Data.CSV.Pretty where

import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Semigroup.Foldable  (intercalate1)
import Data.Monoid              ((<>))
import Data.Separated           (Pesarated1)

import Data.CSV.CSV    (CSV (CSV), Records (getRecords))
import Data.CSV.Field  (Field (QuotedF, UnquotedF))
import Data.CSV.Record
import Text.Between
import Text.Newline    (Newline, newlineString)
import Text.Quote      (Quoted (Quoted), Escaped (SeparatedByEscapes), quoteChar)

prettyField :: Field String String -> String
prettyField f =
  case f of
    QuotedF (Between b (Quoted q (SeparatedByEscapes ss)) t) ->
      let c = [quoteChar q]
          cc = c ++ c
          s = intercalate1 cc ss
      in  concat [b, c, s, c, t]
    UnquotedF s -> s

prettyNewlines :: [Newline] -> String
prettyNewlines = foldMap newlineString

prettyPesarated :: Char -> Pesarated1 Newline (Record String String) -> String
prettyPesarated c =
  bifoldMap newlineString (prettyRecord c)

prettyRecord :: Char -> Record String String -> String
prettyRecord sep (Record fs) =
  intercalate1 [sep] (fmap prettyField fs)

prettyRecords :: Char -> Records String String -> String
prettyRecords c =
  foldMap (prettyPesarated c) . getRecords

prettyCsv :: CSV String String -> String
prettyCsv (CSV c rs e) =
  prettyRecords c rs <> prettyNewlines e

