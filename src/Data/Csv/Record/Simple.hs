{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | This file contains datatypes for Records. A record is a "line" or "row"
-- of a CSV document
module Data.Csv.Record.Simple (
  Record (Record, _fields)
  -- Optics
  , HasRecord (record, fields)
  , fieldsIso
  , singleton
  , HasRecords (records, theRecords)
  , Records (Records, _theRecords)
  , emptyRecords
  , singletonRecords
) where

import Control.Lens       (Lens', Iso, iso, view)
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid        ((<>))
import Data.Separated     (Pesarated (Pesarated), Separated (Separated))
import Data.Traversable   (Traversable (traverse))

import Data.Csv.Field     (Field', MonoField, downmix)
import Text.Newline       (Newline)

-- | A @Record@ is a non-empty collection of Fields, implicitly separated
-- by commas.
newtype Record s =
  Record {
    _fields :: NonEmpty (MonoField s)
  }
  deriving (Eq, Ord, Show)

fieldsIso :: Iso (Record s) (Record a) (NonEmpty (MonoField s)) (NonEmpty (MonoField a))
fieldsIso = iso _fields Record

class HasRecord s t | s -> t where
  record :: Lens' s (Record t)
  fields :: Lens' s (NonEmpty (MonoField t))
  {-# INLINE fields #-}
  fields = record . fields

instance HasRecord (Record s) s where
  record = id
  {-# INLINE fields #-}
  fields = fieldsIso

instance Functor Record where
  fmap f = Record . fmap (fmap f) . _fields

instance Foldable Record where
  foldMap f = foldMap (foldMap f) . _fields

instance Traversable Record where
  traverse f = fmap Record . traverse (traverse f) . _fields

singleton :: Field' s s -> Record s
singleton = Record . pure . downmix

-- | A collection of records, separated and terminated by newlines.
newtype Records s =
  Records { _theRecords :: Pesarated Newline (Record s) }
  deriving (Eq, Ord, Show)

class HasRecords s a | s -> a where
  records :: Lens' s (Records a)
  theRecords :: Lens' s (Pesarated Newline (Record a))
  {-# INLINE theRecords #-}
  theRecords = records . theRecords

instance HasRecords (Records s) s where
  {-# INLINE theRecords #-}
  records = id
  theRecords = iso _theRecords Records

instance Monoid (Records s) where
  mempty = Records mempty
  mappend (Records x) (Records y) = Records (x <> y)

instance Functor Records where
  fmap f = Records . fmap (fmap f) . view theRecords

instance Foldable Records where
  foldMap f = foldMap (foldMap f) . view theRecords

instance Traversable Records where
  traverse f = fmap Records . traverse (traverse f) . view theRecords

emptyRecords :: Records s
emptyRecords = Records mempty

singletonRecords :: Record s -> Newline -> Records s
singletonRecords s n = Records (Pesarated (Separated [(s,n)]))
