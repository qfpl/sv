module Data.Csv.Pretty (
  PrettyConfig' (PrettyConfig', separator', quote, newline, space, string1, string2)
  , PrettyConfig
  , PrettyConfigC
  , setSeparator
  , separator
  , textConfig
  , defaultConfig
  , prettyField
  , prettyRecord
  , prettyPesarated
  , prettyRecords
  , prettyNewlines
  , prettyMonoField
  , prettyFinalRecord
  , prettyNonEmptyRecord
  , prettyNonEmptyString
  , prettyCsv
) where

import Control.Lens             (review)
import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Functor.Identity    (Identity (Identity, runIdentity))
import Data.Monoid              ((<>))
import Data.Semigroup.Foldable  (intercalate1, toNonEmpty)
import Data.Semigroup           (Semigroup)
import Data.Text                (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Text (Builder, fromText)
import Data.Text1               (Text1, _Text1)

import Data.Csv.Csv    (Csv (Csv))
import Data.Csv.Field  (Field (QuotedF, UnquotedF), MonoField, upmix)
import Data.Csv.Record (Record (Record), FinalRecord (unFinal), Records (getRecords), NonEmptyRecord (SingleFieldNER, MultiFieldNER))
import Data.Foldable   (Foldable, fold, toList)
import Text.Between    (Between (Between))
import Text.Newline    (Newline, newlineString)
import Text.Space      (Spaces, spaces)
import Text.Quote      (Quote, Quoted (Quoted), quoteChar)

data PrettyConfig' f s1 s2 m =
  PrettyConfig' {
    separator' :: f m
  , quote     :: Quote -> m
  , newline   :: Newline -> m
  , space     :: Spaces -> m
  , string1   :: s1 -> m
  , string2   :: s2 -> m
  }

instance Functor f => Functor (PrettyConfig' f s1 s2) where
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

setSeparator :: PrettyConfigC s1 s2 m -> Char -> PrettyConfig s1 s2 m
setSeparator (PrettyConfig' s q n sp s1 s2) c =
  PrettyConfig' (Identity (s c)) q n sp s1 s2

separator :: PrettyConfig s1 s2 m -> m
separator = runIdentity . separator'

textConfig :: PrettyConfigC Text1 Text Text
textConfig =
  PrettyConfig' {
    separator' = Text.singleton
  , quote = Text.singleton . quoteChar
  , newline = Text.pack . newlineString
  , space = spaces
  , string1 = review _Text1
  , string2 = id
  }

defaultConfig :: PrettyConfigC Text1 Text Text.Builder
defaultConfig = fmap fromText textConfig

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

prettyNewlines :: Foldable f => f Newline -> String
prettyNewlines = foldMap newlineString

prettyFinalRecord :: (Semigroup m, Monoid m) => PrettyConfig s1 s2 m -> FinalRecord s1 s2 -> m
prettyFinalRecord c = foldMap (prettyNonEmptyRecord c) . unFinal

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
  prettyPesarated c . getRecords

prettyCsv :: (Semigroup m, Monoid m) => PrettyConfigC s1 s2 m -> Csv s1 s2 -> m
prettyCsv config (Csv c rs e) =
  let newConfig = setSeparator config c
  in  prettyRecords newConfig rs <> prettyFinalRecord newConfig e

