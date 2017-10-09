{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Csv.Record.NonEmpty (
  NonEmptyRecord (SingleFieldNER, MultiFieldNER)
  , multiFieldNER
  , nonEmptyRecord
)
where

import Control.Lens       ((^.), Prism', prism, prism', review, preview)
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), first, second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup.Foldable.Extra (toNonEmpty)
import Data.Traversable   (Traversable (traverse))

import Data.Csv.Field         (Field, MonoField)
import Data.Csv.Record.Simple (Record (Record), fields, singleton)
import Data.List.AtLeastTwo   (AtLeastTwo (AtLeastTwo), AsAtLeastTwo (_AtLeastTwo))

-- | A records which is guaranteed not the be the empty string.
-- `s1` is the non-empty string type (Text1 or NonEmpty Char) and `s1` is the
-- corresponding empty-capable type (Text or [Char])
data NonEmptyRecord s1 s2 =
    SingleFieldNER (Field s1 s2)
  | MultiFieldNER (AtLeastTwo (MonoField s2))
  deriving (Eq, Ord, Show)

class AsNonEmptyRecord r s1 s2 | r -> s1 s2 where
  _NonEmptyRecord :: Prism' r (NonEmptyRecord s1 s2)
  _SingleFieldNER :: Prism' r (Field s1 s2)
  _MultiFieldNER :: Prism' r (AtLeastTwo (MonoField s2))
  _SingleFieldNER = _NonEmptyRecord . _SingleFieldNER
  _MultiFieldNER = _NonEmptyRecord . _MultiFieldNER

instance AsNonEmptyRecord (NonEmptyRecord s1 s2) s1 s2 where
  _NonEmptyRecord = id
  _SingleFieldNER =
    prism SingleFieldNER $ \x -> case x of
      SingleFieldNER y -> Right y
      _ -> Left x
  _MultiFieldNER =
    prism MultiFieldNER $ \x -> case x of
      MultiFieldNER y -> Right y
      _ -> Left x

instance Functor (NonEmptyRecord s) where
  fmap = second

instance Foldable (NonEmptyRecord s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord s) where
  traverse = bitraverse pure

instance Bifunctor NonEmptyRecord where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap _ g (MultiFieldNER hs) = MultiFieldNER (fmap (fmap g) hs)

instance Bifoldable NonEmptyRecord where
  bifoldMap f g (SingleFieldNER r) = bifoldMap f g r
  bifoldMap _ g (MultiFieldNER hs) = foldMap (foldMap g) hs

instance Bitraversable NonEmptyRecord where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse _ g (MultiFieldNER hs) = MultiFieldNER <$> traverse (traverse g) hs

multiFieldNER :: MonoField a -> NonEmpty (MonoField a) -> NonEmptyRecord s a
multiFieldNER x xs = MultiFieldNER (AtLeastTwo x xs)

nonEmptyRecord :: Prism' s2 s1 -> Prism' (Record s2) (NonEmptyRecord s1 s2)
nonEmptyRecord p =
  prism'
    (\ner -> case ner of
      SingleFieldNER r -> singleton (first (review p) r)
      MultiFieldNER zs -> Record (toNonEmpty zs))
    (\r -> MultiFieldNER <$> preview _AtLeastTwo (r ^. fields))

