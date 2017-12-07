module Data.Sv.Pretty.Internal (
    prettyField
  , prettyRecord
  , prettyPesarated1
  , prettyRecords
  , prettyCsv
) where

import Control.Lens             (view)
import Data.Bifoldable          (Bifoldable (bifoldMap))
import Data.Monoid              ((<>))
import Data.Semigroup.Foldable  (intercalate1)
import Data.Semigroup           (Semigroup)

import Data.Sv.Sv     (Csv (Csv), Header (Header))
import Data.Sv.Field  (Field (QuotedF, UnquotedF))
import Data.Sv.Pretty.Config (PrettyConfigC, PrettyConfig, string, setSeparator, separator, newline, space, quote)
import Data.Sv.Record (Record (Record), Records, HasRecords (theRecords))
import Data.Foldable   (fold)
import Text.Between    (Between (Between))
import Text.Newline    (Newline)
import Text.Quote      (Quoted (Quoted))

prettyField :: (Monoid m, Semigroup m) => PrettyConfig s m -> Field s -> m
prettyField config f =
  case f of
    QuotedF (Between b (Quoted q ss) t) ->
      let c = quote config q
          cc = c <> c
          spc = space config
          s = bifoldMap (const cc) (string config) ss
      in  fold [spc b, c, s, c, spc t]
    UnquotedF s -> string config s

prettyPesarated1 :: (Bifoldable p, Semigroup m, Monoid m) => PrettyConfig s m -> p Newline (Record s) -> m
prettyPesarated1 c =
  bifoldMap (newline c) (prettyRecord c)

prettyRecord :: (Semigroup m, Monoid m) => PrettyConfig s m -> Record s -> m
prettyRecord c (Record fs) =
  let sep = separator c
  in  intercalate1 sep (fmap (prettyField c) fs)

prettyHeader :: (Semigroup m, Monoid m) => PrettyConfig s m -> Header s -> m
prettyHeader c (Header r n) = prettyRecord c r <> newline c n

prettyRecords :: (Semigroup m, Monoid m) => PrettyConfig s m -> Records s -> m
prettyRecords c =
  foldMap (prettyPesarated1 c) . view theRecords

prettyCsv :: (Semigroup m, Monoid m) => PrettyConfigC s m -> Csv s -> m
prettyCsv config (Csv c h rs e) =
  let newConfig = setSeparator config c
  in  foldMap (prettyHeader newConfig) h <> prettyRecords newConfig rs <> foldMap (newline newConfig) e

