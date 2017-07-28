module Data.CSV.Pretty where

import Data.Semigroup.Foldable  (intercalate1)
import Data.List                (intercalate)

import Data.CSV.Field  (Field (QuotedF, UnquotedF))
import Data.CSV.Record
import Text.Between
import Text.Quote      (Quoted (Quoted), Escaped (SeparatedByEscapes), quoteChar)

prettyField :: Field String String -> String
prettyField f =
  case f of
    QuotedF (Between b t (Quoted q (SeparatedByEscapes ss))) ->
      let c = [quoteChar q]
          cc = c ++ c
          s = intercalate1 cc ss
      in  concat [b, c, s, c, t]
    UnquotedF s -> s

prettyRecord :: Char -> Record String String -> String
prettyRecord sep (Record fs) =
  intercalate [sep] (fmap prettyField fs)

