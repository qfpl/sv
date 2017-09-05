module Data.CSV.Pretty where

import Control.Lens             (re, review, Prism', Optic', Choice)
import Data.Text.Lens           (IsText (packed))
import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Bifunctor           (first)
import Data.Functor.Identity    (Identity (Identity, runIdentity))
import Data.List.NonEmpty       ((<|))
import Data.Monoid              ((<>))
import Data.Semigroup.Foldable  (intercalate1)
import Data.Semigroup           (Semigroup)
import Data.Text                (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Text (Builder, fromText)
import Data.Text1               (AsText1, Text1, _Text1)

import Data.CSV.CSV    (CSV (CSV), FinalRecord (unFinal), Records (getRecords))
import Data.CSV.Field  (Field (QuotedF, UnquotedF), MonoField (MonoField, getField))
import Data.CSV.Record
import Data.Foldable   (Foldable, fold, toList)
import Data.NonEmptyString (NonEmptyString)
import Text.Between
import Text.Newline    (Newline, newlineString)
import Text.Quote      (Quote, Quoted (Quoted), Escaped (SeparatedByEscapes), quoteChar)

data PrettyConfig' f space s1 s2 m =
  PrettyConfig' {
    separator' :: f m
  , quote     :: Quote -> m
  , newline   :: Newline -> m
  , space     :: space -> m
  , string1   :: s1 -> m
  , string2   :: s2 -> m
  }

instance Functor f => Functor (PrettyConfig' f space s1 s2) where
  fmap f (PrettyConfig' s q n sp s1 s2) =
    PrettyConfig' {
      separator' = fmap f s
    , quote = fmap f q
    , newline = fmap f n
    , space = fmap f sp
    , string1 = fmap f s1
    , string2 = fmap f s2
    }

type PrettyConfig = PrettyConfig' Identity
type PrettyConfigC = PrettyConfig' ((->) Char)

setSeparator :: PrettyConfigC space s1 s2 m -> Char -> PrettyConfig space s1 s2 m
setSeparator (PrettyConfig' s q n sp s1 s2) c =
  PrettyConfig' (Identity (s c)) q n sp s1 s2

separator :: PrettyConfig space s1 s2 m -> m
separator = runIdentity . separator'

textConfig :: PrettyConfigC Text Text1 Text Text
textConfig =
  PrettyConfig' {
    separator' = Text.singleton
  , quote = Text.singleton . quoteChar
  , newline = Text.pack . newlineString
  , space = id
  , string1 = review _Text1
  , string2 = id
  }

defaultConfig :: PrettyConfigC Text Text1 Text Text.Builder
defaultConfig = fmap fromText textConfig

prettyField :: (Monoid m, Semigroup m) => PrettyConfig space s1 s2 m -> Field space s1 s2 -> m
prettyField config f =
  case f of
    QuotedF (Between b (Quoted q (SeparatedByEscapes ss)) t) ->
      let c = quote config q
          cc = c <> c
          spc = space config
          s = intercalate1 cc (fmap (string2 config) ss)
      in  fold [spc b, c, s, c, spc t]
    UnquotedF s -> string1 config s

prettyMonoField :: (Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> MonoField space s2 -> m
prettyMonoField c =
  prettyField c {string1 = string2 c} . getField

prettyNewlines :: Foldable f => f Newline -> String
prettyNewlines = foldMap newlineString

prettyFinalRecord :: (Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> FinalRecord space s1 s2 -> m
prettyFinalRecord c = foldMap (prettyNonEmptyRecord c) . unFinal

prettyPesarated :: (Bifoldable p, Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> p Newline (Record space s2) -> m
prettyPesarated c =
  bifoldMap (newline c) (prettyRecord c)

prettyRecord :: (Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> Record space s2 -> m
prettyRecord c (Record fs) =
  let sep = separator c
  in  intercalate1 sep (fmap (prettyMonoField c) fs)

prettyNonEmptyString :: Foldable f => f Char -> String
prettyNonEmptyString = toList

prettyNonEmptyRecord :: (Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> NonEmptyRecord space s1 s2 -> m
prettyNonEmptyRecord c (SingleFieldNER f) =
  prettyField c f
prettyNonEmptyRecord c (MultiFieldNER f fs) =
  prettyRecord c (Record (f <| fs))

prettyRecords :: (Semigroup m, Monoid m) => PrettyConfig space s1 s2 m -> Records space s2 -> m
prettyRecords c =
  prettyPesarated c . getRecords

prettyCsv :: (Semigroup m, Monoid m) => PrettyConfigC space s1 s2 m -> CSV space s1 s2 -> m
prettyCsv config (CSV c rs e) =
  let newConfig = setSeparator config c
  in  prettyRecords newConfig rs <> prettyFinalRecord newConfig e

