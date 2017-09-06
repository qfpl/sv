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
data Csv spc s1 s2 =
  Csv {
    separator :: Char
  , initialRecords :: Records spc s2
  , finalRecord :: FinalRecord spc s1 s2
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> FinalRecord spc s1 s2 -> Records spc s2 -> Csv spc s1 s2
mkCsv c ns rs = Csv c rs ns

mkCsv' :: Char -> FinalRecord spc s1 s2 -> Pesarated Newline (Record spc s2) -> Csv spc s1 s2
mkCsv' c ns = mkCsv c ns . Records

instance Functor (Csv n spc) where
  fmap = second

instance Foldable (Csv n spc) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Csv n spc) where
  traverse = bitraverse pure

instance Bifunctor (Csv n) where
  bimap f g (Csv s rs e) = Csv s (fmap g rs) (bimap f g e)

instance Bifoldable (Csv n) where
  bifoldMap f g (Csv _ rs e) = foldMap g rs <> bifoldMap f g e

instance Bitraversable (Csv n) where
  bitraverse f g (Csv s rs e) = Csv s <$> traverse g rs <*> bitraverse f g e
