module Data.CSV.CSV where

import Data.Bifunctor   (Bifunctor (bimap))
import Data.Foldable    (Foldable (foldMap))
import Data.Functor     (Functor (fmap), (<$>))
import Data.Traversable (Traversable (traverse))

import Data.CSV.Record

data CSV spc str =
  CSV {
    separator :: Char
  , records :: [Record spc str]
  }
  deriving (Eq, Ord, Show)

instance Functor (CSV spc) where
  fmap f (CSV s rs) = CSV s (fmap (fmap f) rs)

instance Foldable (CSV spc) where
  foldMap f = foldMap (foldMap f) . records

instance Traversable (CSV spc) where
  traverse f (CSV s rs) = CSV s <$> traverse (traverse f) rs

instance Bifunctor CSV where
  bimap f g (CSV s rs) = CSV s (map (bimap f g) rs)

