module Data.CSV.CSV where

import Data.Bifunctor   (Bifunctor (bimap))
import Data.Foldable    (Foldable (foldMap))
import Data.Functor     (Functor (fmap), (<$>))
import Data.Traversable (Traversable (traverse))

import Data.CSV.Record  (Record)
import Data.Separated   (Pesarated1)
import Text.Newline

data CSV spc str =
  CSV {
    separator :: Char
  , records :: Maybe (Pesarated1 Newline (Record spc str))
  , end :: [Newline]
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> [Newline] -> Maybe (Pesarated1 Newline (Record spc str)) -> CSV spc str
mkCsv c = flip (CSV c)

instance Functor (CSV spc) where
  fmap f (CSV s rs e) = CSV s (fmap (fmap (fmap f)) rs) e

instance Foldable (CSV spc) where
  foldMap f = foldMap (foldMap (foldMap f)) . records

instance Traversable (CSV spc) where
  traverse f (CSV s rs e) = flip (CSV s) e <$> traverse (traverse (traverse f)) rs

instance Bifunctor CSV where
  bimap f g (CSV s rs e) = flip (CSV s) e $ fmap (fmap (bimap f g)) rs

