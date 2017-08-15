module Data.CSV.Record where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Field   (Field)

newtype Record spc str =
  Record {
    fields :: [Field spc str]
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

