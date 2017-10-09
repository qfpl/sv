module Data.Csv.Pretty.Internal (
    prettyField
  , prettyRecord
  , prettyPesarated
  , prettyRecords
  , prettyMonoField
  , prettyFinalRecord
  , prettyNonEmptyRecord
  , prettyNonEmptyString
  , prettyCsv
) where

import Control.Lens             (view)
import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Monoid              ((<>))
import Data.Semigroup.Foldable  (intercalate1)
import Data.Semigroup.Foldable.Extra (toNonEmpty)
import Data.Semigroup           (Semigroup)

import Data.Csv.Csv    (Csv (Csv))
import Data.Csv.Field  (Field (QuotedF, UnquotedF), MonoField, upmix)
import Data.Csv.Pretty.Config (PrettyConfigC, PrettyConfig, string1, string2, setSeparator, separator, newline, space, quote)
import Data.Csv.Record (Record (Record), FinalRecord, HasFinalRecord (maybeNer), Records, HasRecords (theRecords), NonEmptyRecord (SingleFieldNER, MultiFieldNER))
import Data.Foldable   (Foldable, fold, toList)
import Text.Between    (Between (Between))
import Text.Newline    (Newline)
import Text.Quote      (Quoted (Quoted))

prettyField :: (Monoid m, Semigroup m) => PrettyConfig s1 s2 m -> Field s1 s2 -> m
prettyField config f =
  case f of
    QuotedF (Between b (Quoted q ss) t) ->
      let c = quote config q
          cc = c <> c
          spc = space config
          s = bifoldMap (const cc) (string2 config) ss
      in  fold [spc b, c, s, c, spc t]
    UnquotedF s -> string1 config s

prettyMonoField :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> MonoField s2 -> m
prettyMonoField c =
  prettyField c {string1 = string2 c} . upmix

prettyFinalRecord :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> FinalRecord s1 s2 -> m
prettyFinalRecord c = foldMap (prettyNonEmptyRecord c) . view maybeNer

prettyPesarated :: (Bifoldable p, Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> p Newline (Record s2) -> m
prettyPesarated c =
  bifoldMap (newline c) (prettyRecord c)

prettyRecord :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> Record s2 -> m
prettyRecord c (Record fs) =
  let sep = separator c
  in  intercalate1 sep (fmap (prettyMonoField c) fs)

prettyNonEmptyString :: Foldable f => f Char -> String
prettyNonEmptyString = toList

prettyNonEmptyRecord :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> NonEmptyRecord s1 s2 -> m
prettyNonEmptyRecord c (SingleFieldNER f) =
  prettyField c f
prettyNonEmptyRecord c (MultiFieldNER fs) =
  prettyRecord c (Record (toNonEmpty fs))

prettyRecords :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> Records s2 -> m
prettyRecords c =
  prettyPesarated c . view theRecords

prettyCsv :: (Semigroup m, Monoid m) => PrettyConfigC s1 s2 m -> Csv s1 s2 -> m
prettyCsv config (Csv c rs e) =
  let newConfig = setSeparator config c
  in  prettyRecords newConfig rs <> prettyFinalRecord newConfig e

