module Data.CSV.Record where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid        ((<>))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Field     (Field, MonoField)

newtype Record spc s =
  Record {
    fields :: NonEmpty (MonoField spc s)
  }
  deriving (Eq, Ord, Show)

instance Functor (Record spc) where
  fmap f = Record . fmap (fmap f) . fields

instance Foldable (Record spc) where
  foldMap f = foldMap (foldMap f) . fields

instance Traversable (Record spc) where
  traverse f = fmap Record . traverse (traverse f) . fields

instance Bifunctor Record where
  bimap f g = Record . fmap (bimap f g) . fields

instance Bifoldable Record where
  bifoldMap f g = foldMap (bifoldMap f g) . fields

instance Bitraversable Record where
  bitraverse f g = fmap Record . traverse (bitraverse f g) . fields

data NonEmptyRecord spc s1 s2 =
    SingleFieldNER (Field spc s1 s2)
  | MultiFieldNER (MonoField spc s2) (NonEmpty (MonoField spc s2))
  deriving (Eq, Ord, Show)

instance Functor (NonEmptyRecord n s) where
  fmap = second

instance Foldable (NonEmptyRecord n s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord n s) where
  traverse = bitraverse pure

instance Bifunctor (NonEmptyRecord n) where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap _ g (MultiFieldNER h hs) = MultiFieldNER (fmap g h) (fmap (fmap g) hs)

instance Bifoldable (NonEmptyRecord n) where
  bifoldMap f g (SingleFieldNER r) = bifoldMap f g r
  bifoldMap _ g (MultiFieldNER h hs) = foldMap g h <> foldMap (bifoldMap (const mempty) g) hs

instance Bitraversable (NonEmptyRecord n) where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse _ g (MultiFieldNER h hs) = MultiFieldNER <$> traverse g h <*> traverse (bitraverse pure g) hs

