{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE RankNTypes #-}

-- | This file defines a datatype for a complete CSV document.
-- The datatype preserves information so that the original CSV
-- text can be recovered.
module Data.Csv.Csv (
  Csv (Csv, _separator, _initialRecords, _finalRecord)
  , HasCsv (csv, finalRecord, initialRecords, separator)
  , mkCsv
  , mkCsv'
  , empty
  , unconsRecord
) where

import Control.Lens       ((^.), Lens', Prism', review)
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Monoid        ((<>))
import Data.Separated     (Pesarated (Pesarated), Separated (Separated))
import Data.Traversable   (Traversable (traverse))

import Data.Csv.Record    (Record, Records (Records), HasRecords (theRecords), FinalRecord, HasFinalRecord (maybeNer), noFinal, nonEmptyRecord)
import Text.Newline       (Newline)

-- | 'Csv' is a whitespace-preserving data type for separated values.
--   Often the separator is a comma, but this type does not make that
--   assumption so that it can be used for pipe- or tab-separated values
--   as well.
data Csv s1 s2 =
  Csv {
    _separator :: Char
  , _initialRecords :: Records s2
  , _finalRecord :: FinalRecord s1 s2
  }
  deriving (Eq, Ord, Show)

class HasCsv c s1 s2 | c -> s1 s2 where
  csv :: Lens' c (Csv s1 s2)
  finalRecord :: Lens' c (FinalRecord s1 s2)
  {-# INLINE finalRecord #-}
  initialRecords :: Lens' c (Records s2)
  {-# INLINE initialRecords #-}
  separator :: Lens' c Char
  {-# INLINE separator #-}
  finalRecord = csv . finalRecord
  initialRecords = csv . initialRecords
  separator = csv . separator

instance HasCsv (Csv s1 s2) s1 s2 where
  {-# INLINE finalRecord #-}
  {-# INLINE initialRecords #-}
  {-# INLINE separator #-}
  csv = id
  finalRecord f (Csv x1 x2 x3)
    = fmap (Csv x1 x2) (f x3)
  initialRecords f (Csv x1 x2 x3)
    = fmap (\y -> Csv x1 y x3) (f x2)
  separator f (Csv x1 x2 x3)
    = fmap (\y -> Csv y x2 x3) (f x1)

-- | Convenience constructor for CSV
mkCsv :: Char -> FinalRecord s1 s2 -> Records s2 -> Csv s1 s2
mkCsv c ns rs = Csv c rs ns

-- | Convenience constructor for CSV
mkCsv' :: Char -> FinalRecord s1 s2 -> Pesarated Newline (Record s2) -> Csv s1 s2
mkCsv' c ns = mkCsv c ns . Records

empty :: Char -> Csv s1 s2
empty c = Csv c mempty noFinal

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

unconsRecord :: Prism' s2 s1 -> Csv s1 s2 -> Maybe ((Record s2, Maybe Newline), Csv s1 s2)
unconsRecord p (Csv sep initial final) =
  case initial ^. theRecords of
    Pesarated (Separated []) -> case final ^. maybeNer of
      Nothing -> Nothing
      Just r -> Just ((review (nonEmptyRecord p) r, Nothing), empty sep)
    Pesarated (Separated ((r,n):rs)) -> Just ((r,Just n), Csv sep (Records (Pesarated (Separated rs))) final)

