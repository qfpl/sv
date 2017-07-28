module Data.CSV.Field where

import Data.Bifunctor   (Bifunctor (bimap))
import Data.Foldable    (Foldable (foldMap))
import Data.Functor     (Functor (fmap))
import Data.Traversable (Traversable (traverse))

import Text.Between (Between)
import Text.Quote   (Quoted)

data Field spc str =
    UnquotedF str
  | QuotedF (Between spc (Quoted str))
  deriving (Eq, Ord, Show)

instance Functor (Field spc) where
  fmap f fi = case fi of
    UnquotedF s -> UnquotedF (f s)
    QuotedF b -> QuotedF (fmap (fmap f) b)

instance Foldable (Field spc) where
  foldMap f fi = case fi of
    UnquotedF s -> f s
    QuotedF b -> foldMap (foldMap f) b

instance Traversable (Field spc) where
  traverse f fi = case fi of
    UnquotedF s -> fmap UnquotedF (f s)
    QuotedF b -> fmap QuotedF (traverse (traverse f) b)

instance Bifunctor Field where
  bimap f g fi = case fi of
    UnquotedF s -> UnquotedF (g s)
    QuotedF b -> QuotedF (bimap f (fmap g) b)

