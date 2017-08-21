module Data.CSV.Pretty where

import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Bifunctor           (first)
import Data.List.NonEmpty       ((<|))
import Data.Monoid              ((<>))
import Data.Semigroup.Foldable  (intercalate1)

import Data.CSV.CSV    (CSV (CSV), FinalRecord (unFinal), Records (getRecords))
import Data.CSV.Field  (Field (QuotedF, UnquotedF), MonoField (MonoField, getField))
import Data.CSV.Record
import Data.Foldable   (Foldable, toList)
import Data.NonEmptyString (NonEmptyString)
import Text.Between
import Text.Newline    (Newline, newlineString)
import Text.Quote      (Quoted (Quoted), Escaped (SeparatedByEscapes), quoteChar)

prettyField :: Field String String String -> String
prettyField f =
  case f of
    QuotedF (Between b (Quoted q (SeparatedByEscapes ss)) t) ->
      let c = [quoteChar q]
          cc = c ++ c
          s = intercalate1 cc ss
      in  concat [b, c, s, c, t]
    UnquotedF s -> s

prettyMonoField :: MonoField String String -> String
prettyMonoField = prettyField . getField

prettyNewlines :: Foldable f => f Newline -> String
prettyNewlines = foldMap newlineString

prettyFinalRecord :: Char -> FinalRecord String NonEmptyString String -> String
prettyFinalRecord c = foldMap (prettyNonEmptyRecord c) . unFinal

prettyPesarated :: Bifoldable p => Char -> p Newline (Record String String) -> String
prettyPesarated c =
  bifoldMap newlineString (prettyRecord c)

prettyRecord :: Char -> Record String String -> String
prettyRecord sep (Record fs) =
  intercalate1 [sep] (fmap prettyMonoField fs)

prettyNonEmptyString :: Foldable f => f Char -> String
prettyNonEmptyString = toList

prettyNonEmptyRecord :: Char -> NonEmptyRecord String NonEmptyString String -> String
prettyNonEmptyRecord _ (SingleFieldNER f) =
  bifoldMap prettyNonEmptyString id f
prettyNonEmptyRecord c (MultiFieldNER f fs) =
  prettyRecord c (Record (f <| fs))

prettyRecords :: Char -> Records String String -> String
prettyRecords c =
  prettyPesarated c . getRecords

prettyCsv :: CSV String NonEmptyString String -> String
prettyCsv (CSV c rs e) =
  prettyRecords c (first toList rs) <> prettyFinalRecord c e

