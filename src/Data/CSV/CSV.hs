module Data.CSV.CSV where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Record    (Record)
import Data.Separated     (Pesarated1)
import Text.Newline       (Newline)

-- | Whitespace-preserving CSV data type
data CSV spc str =
  CSV {
    separator :: Char
  , records :: Records spc str
  , end :: Maybe Newline
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> Maybe Newline -> Records spc str -> CSV spc str
mkCsv c ns rs = CSV c rs ns

mkCsv' :: Char -> Maybe Newline -> Pesarated1 Newline (Record spc str) -> CSV spc str
mkCsv' c ns = mkCsv c ns . Records

instance Functor (CSV spc) where
  fmap f (CSV s rs e) = CSV s (fmap f rs) e

instance Foldable (CSV spc) where
  foldMap f = foldMap f . records

instance Traversable (CSV spc) where
  traverse f (CSV s rs e) = mkCsv s e <$> traverse f rs

instance Bifunctor CSV where
  bimap f g (CSV s rs e) = mkCsv s e $ bimap f g rs

instance Bifoldable CSV where
  bifoldMap f g = bifoldMap f g . records

instance Bitraversable CSV where
  bitraverse f g (CSV s rs e) = mkCsv s e <$> bitraverse f g rs

-- | Newtype for records
newtype Records spc str =
  Records { getRecords :: Pesarated1 Newline (Record spc str) }
  deriving (Eq, Ord, Show)

instance Functor (Records spc) where
  fmap f (Records rs) = Records (fmap (fmap f) rs)

instance Foldable (Records spc) where
  foldMap f = foldMap (foldMap f) . getRecords

instance Traversable (Records spc) where
  traverse f = fmap Records . traverse (traverse f) . getRecords

instance Bifunctor Records where
  bimap f g = Records . fmap (bimap f g) . getRecords

instance Bifoldable Records where
  bifoldMap f g = foldMap (bifoldMap f g) . getRecords

instance Bitraversable Records where
  bitraverse f g = fmap Records . traverse (bitraverse f g) . getRecords

