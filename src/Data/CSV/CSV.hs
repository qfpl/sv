module Data.CSV.CSV where

import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Monoid        ((<>))
import Data.Traversable   (Traversable (traverse))

import Data.CSV.Record    (NonEmptyRecord, Record)
import Data.Separated     (Pesarated)
import Data.Trifoldable   (Trifoldable (trifoldMap))
import Data.Trifunctor    (Trifunctor (trimap))
import Data.Triversable   (Triversable (triverse))
import Text.Newline       (Newline)

-- | Whitespace-preserving CSV data type
data CSV spc s1 s2 =
  CSV {
    separator :: Char
  , recordSet :: RecordSet spc s1 s2
  }
  deriving (Eq, Ord, Show)

mkCsv :: Char -> FinalRecord spc s1 s2 -> Records spc s2 -> CSV spc s1 s2
mkCsv c ns rs = CSV c (RecordSet rs ns)

mkCsv' :: Char -> FinalRecord spc s1 s2 -> Pesarated Newline (Record spc s2 s2) -> CSV spc s1 s2
mkCsv' c ns = mkCsv c ns . Records

instance Functor (CSV n spc) where
  fmap = second

instance Foldable (CSV n spc) where
  foldMap = bifoldMap (const mempty)

instance Traversable (CSV n spc) where
  traverse = bitraverse pure

instance Bifunctor (CSV n) where
  bimap f g (CSV s rs) = CSV s (bimap f g rs)

instance Bifoldable (CSV n) where
  bifoldMap f g (CSV _ rs) = bifoldMap f g rs

instance Bitraversable (CSV n) where
  bitraverse f g (CSV s rs) = CSV s <$> bitraverse f g rs


-- ??
data RecordSet spc s1 s2 =
  RecordSet {
    records :: Records spc s2
  , end :: FinalRecord spc s1 s2
  }
  deriving (Eq, Ord, Show)

instance Bifunctor (RecordSet spc) where
  bimap f g (RecordSet rs e) = RecordSet (fmap g rs) (bimap f g e)

instance Bifoldable (RecordSet spc) where
  bifoldMap f g (RecordSet rs e) = foldMap g rs <> bifoldMap f g e

instance Bitraversable (RecordSet spc) where
  bitraverse f g (RecordSet rs e) = RecordSet <$> traverse g rs <*> bitraverse f g e

-- | Newtype for records
newtype Records spc s =
  Records { getRecords :: Pesarated Newline (Record spc s s) }
  deriving (Eq, Ord, Show)

instance Functor (Records spc) where
  fmap f = Records . fmap (bimap f f) . getRecords

instance Foldable (Records spc) where
  foldMap f = foldMap (foldMap f) . getRecords

instance Traversable (Records spc) where
  traverse f = fmap Records . traverse (bitraverse f f) . getRecords

instance Bifunctor Records where
  bimap f g = Records . fmap (trimap f g g) . getRecords

instance Bifoldable Records where
  bifoldMap f g = foldMap (trifoldMap f g g) . getRecords

instance Bitraversable Records where
  bitraverse f g = fmap Records . traverse (triverse f g g) . getRecords

newtype FinalRecord spc s1 s2 =
  FinalRecord { unFinal :: Maybe (NonEmptyRecord spc s1 s2) }
  deriving (Eq, Ord, Show)

instance Functor (FinalRecord n a) where
  fmap = second

instance Foldable (FinalRecord n a) where
  foldMap = bifoldMap (const mempty)

instance Traversable (FinalRecord n a) where
  traverse = bitraverse pure

instance Bifunctor (FinalRecord n) where
  bimap f g = FinalRecord . fmap (bimap f g) . unFinal

instance Bifoldable (FinalRecord n) where
  bifoldMap f g = foldMap (bifoldMap f g) . unFinal

instance Bitraversable (FinalRecord n) where
  bitraverse f g = fmap FinalRecord . traverse (bitraverse f g) . unFinal

noFinal :: FinalRecord nestr spc str
noFinal = FinalRecord Nothing

