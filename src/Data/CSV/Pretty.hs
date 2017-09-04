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

data PrettyConfig s1 s2 =
  PrettyConfig {
    separator :: Char
  , string1 :: s1 -> String
  , string2 :: s2 -> String
  }


prettyField :: (s1 -> String) -> (s2 -> String) -> Field String s1 s2 -> String
prettyField str1 str2 f =
  case f of
    QuotedF (Between b (Quoted q (SeparatedByEscapes ss)) t) ->
      let c = [quoteChar q]
          cc = c <> c
          s = intercalate1 cc (fmap str2 ss)
      in  concat [b, c, s, c, t]
    UnquotedF s -> str1 s

prettyMonoField :: PrettyConfig s1 s2 -> MonoField String s2 -> String
prettyMonoField c =
  let str = string2 c in prettyField str str . getField

prettyNewlines :: Foldable f => f Newline -> String
prettyNewlines = foldMap newlineString

prettyFinalRecord :: PrettyConfig s1 s2 -> FinalRecord String s1 s2 -> String
prettyFinalRecord c = foldMap (prettyNonEmptyRecord c) . unFinal

prettyPesarated :: Bifoldable p => PrettyConfig s1 s2 -> p Newline (Record String s2) -> String
prettyPesarated c =
  bifoldMap newlineString (prettyRecord c)

prettyRecord :: PrettyConfig s1 s2 -> Record String s2 -> String
prettyRecord c (Record fs) =
  let sep = separator c
      str = string2 c
  in  intercalate1 [sep] (fmap (prettyMonoField c) fs)

prettyNonEmptyString :: Foldable f => f Char -> String
prettyNonEmptyString = toList

prettyNonEmptyRecord :: PrettyConfig s1 s2 -> NonEmptyRecord String s1 s2 -> String
prettyNonEmptyRecord c (SingleFieldNER f) =
  prettyField (string1 c) (string2 c) f
prettyNonEmptyRecord c (MultiFieldNER f fs) =
  prettyRecord c (Record (f <| fs))

prettyRecords :: PrettyConfig s1 s2 -> Records String s2 -> String
prettyRecords c =
  prettyPesarated c . getRecords

prettyCsv :: (s1 -> String) -> (s2 -> String) -> CSV String s1 s2 -> String
prettyCsv s1 s2 (CSV c rs e) =
  let config = PrettyConfig c s1 s2
  in  prettyRecords config (first toList rs) <> prettyFinalRecord config e

