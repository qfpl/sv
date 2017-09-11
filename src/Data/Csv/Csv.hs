-- | This file defines a datatype for a complete CSV document.
-- The datatype preserves information so that the original CSV
-- text can be recovered.
module Data.Csv.Csv (
  Csv (Csv, separator, initialRecords, finalRecord)
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

import Data.Csv.Record    (Record, Records (Records), FinalRecord)
import Data.Separated     (Pesarated)
import Text.Newline       (Newline)

-- | Whitespace-preserving Csv data type
data Csv s1 s2 =
  Csv {
    separator :: Char
  , initialRecords :: Records s2
  , finalRecord :: FinalRecord s1 s2
  }
  deriving (Eq, Ord, Show)

-- | Convenience constructor for CSV
mkCsv :: Char -> FinalRecord s1 s2 -> Records s2 -> Csv s1 s2
mkCsv c ns rs = Csv c rs ns

-- | Convenience constructor for CSV
mkCsv' :: Char -> FinalRecord s1 s2 -> Pesarated Newline (Record s2) -> Csv s1 s2
mkCsv' c ns = mkCsv c ns . Records

instance Functor (Csv s) where
  fmap = second

instance Foldable (Csv s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Csv s) where
  traverse = bitraverse pure

instance Bifunctor Csv where
  bimap f g (Csv s rs e) = Csv s (fmap g rs) (bimap f g e)

instance Bifoldable Csv where
  bifoldMap f g (Csv _ rs e) = foldMap g rs <> bifoldMap f g e

instance Bitraversable Csv where
  bitraverse f g (Csv s rs e) = Csv s <$> traverse g rs <*> bitraverse f g e

