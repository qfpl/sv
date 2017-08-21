module Data.CSV.Record where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Trifoldable   (Trifoldable (trifoldMap))
import Data.Trifunctor    (Trifunctor (trimap))
import Data.Triversable   (Triversable (triverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid        ((<>))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Field   (Field)

newtype Record spc s1 s2 =
  Record {
    fields :: NonEmpty (Field spc s1 s2)
  }
  deriving (Eq, Ord, Show)

instance Functor (Record spc s1) where
  fmap f = Record . fmap (fmap f) . fields

instance Foldable (Record spc s1) where
  foldMap f = foldMap (foldMap f) . fields

instance Traversable (Record spc s1) where
  traverse f = fmap Record . traverse (traverse f) . fields

instance Bifunctor (Record spc) where
  bimap f g = Record . fmap (bimap f g) . fields

instance Bifoldable (Record spc) where
  bifoldMap f g = foldMap (bifoldMap f g) . fields

instance Bitraversable (Record spc) where
  bitraverse f g = fmap Record . traverse (bitraverse f g) . fields

instance Trifunctor Record where
  trimap f g h = Record . fmap (trimap f g h) . fields

instance Trifoldable Record where
  trifoldMap f g h = foldMap (trifoldMap f g h) . fields

instance Triversable Record where
  triverse f g h = fmap Record . traverse (triverse f g h) . fields

data NonEmptyRecord spc s1 s2 =
    SingleFieldNER (Field spc s1 s2)
  | MultiFieldNER (Field spc s2 s2) (NonEmpty (Field spc s2 s2))
  deriving (Eq, Ord, Show)

instance Functor (NonEmptyRecord n s) where
  fmap = second

instance Foldable (NonEmptyRecord n s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (NonEmptyRecord n s) where
  traverse = bitraverse pure

instance Bifunctor (NonEmptyRecord n) where
  bimap f g (SingleFieldNER r) = SingleFieldNER (bimap f g r)
  bimap f g (MultiFieldNER h hs) = MultiFieldNER (bimap g g h) (fmap (bimap g g) hs)

instance Bifoldable (NonEmptyRecord n) where
  bifoldMap f _ (SingleFieldNER r) = bifoldMap f (const mempty) r
  bifoldMap f g (MultiFieldNER h hs) = bifoldMap g g h <> foldMap (bifoldMap g g) hs

instance Bitraversable (NonEmptyRecord n) where
  bitraverse f g (SingleFieldNER r) = SingleFieldNER <$> bitraverse f g r
  bitraverse f g (MultiFieldNER h hs) = MultiFieldNER <$> bitraverse g g h <*> traverse (bitraverse g g) hs

instance Trifunctor NonEmptyRecord where
  trimap f g h (SingleFieldNER r) = SingleFieldNER (trimap f g h r)
  trimap f g h (MultiFieldNER x xs) = MultiFieldNER (trimap f h h x) (fmap (trimap f h h) xs)

instance Trifoldable NonEmptyRecord where
  trifoldMap f g h (SingleFieldNER r) = trifoldMap f g h r
  trifoldMap f g h (MultiFieldNER x xs) = trifoldMap f h h x <> foldMap (trifoldMap f h h) xs

instance Triversable NonEmptyRecord where
  triverse f g h (SingleFieldNER r) = SingleFieldNER <$> triverse f g h r
  triverse f g h (MultiFieldNER x xs) = MultiFieldNER <$> triverse f h h x <*> traverse (triverse f h h) xs

