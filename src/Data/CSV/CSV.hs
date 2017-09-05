module Data.CSV.CSV (
  CSV (CSV, separator, initialRecords, finalRecord)
  , mkCsv
  , mkCsv'
) where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Monoid        ((<>))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Record    (Record, Records (Records), FinalRecord)
import Data.Separated     (Pesarated)
import Text.Newline       (Newline)

-- | Whitespace-preserving CSV data type
data CSV spc s1 s2 =
  CSV {
    separator :: Char
  , initialRecords :: Records spc s2
  , finalRecord :: FinalRecord spc s1 s2
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> FinalRecord spc s1 s2 -> Records spc s2 -> CSV spc s1 s2
mkCsv c ns rs = CSV c rs ns

mkCsv' :: Char -> FinalRecord spc s1 s2 -> Pesarated Newline (Record spc s2) -> CSV spc s1 s2
mkCsv' c ns = mkCsv c ns . Records

instance Functor (CSV n spc) where
  fmap = second

instance Foldable (CSV n spc) where
  foldMap = bifoldMap (const mempty)

instance Traversable (CSV n spc) where
  traverse = bitraverse pure

instance Bifunctor (CSV n) where
  bimap f g (CSV s rs e) = CSV s (fmap g rs) (bimap f g e)

instance Bifoldable (CSV n) where
  bifoldMap f g (CSV _ rs e) = foldMap g rs <> bifoldMap f g e

instance Bitraversable (CSV n) where
  bitraverse f g (CSV s rs e) = CSV s <$> traverse g rs <*> bitraverse f g e
