{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Sv.Syntax.Record
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This module contains datatypes for Records. A record is a "line" or "row"
of a CSV document
-}
module Data.Sv.Syntax.Record (
  Record (Record, _fields)
  -- Optics
  , HasRecord (record, spacedFields)
  , recordSpacedFieldsIso
  , emptyRecord
  , singleField
  , recordNel
  , Records (EmptyRecords, Records)
  , HasRecords (records, traverseRecords, traverseNewlines)
  , _EmptyRecords
  , _NonEmptyRecords
  , mkRecords
  , singleRecord
  , recordList
) where

import Control.DeepSeq (NFData)
import Control.Lens (Lens, Lens', Iso, Prism, Prism', Traversal', _1, _2, beside, iso, prism, prism', toListOf)
import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup)
import Data.Traversable (Traversable (traverse))
import GHC.Generics (Generic)

import Data.Sv.Syntax.Field (SpacedField, Field (Unquoted), HasFields (fields))
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as V
import Text.Newline (Newline)
import Text.Space (Spaced, spacedValue)

-- | A @Record@ is a non-empty collection of Fields, implicitly separated
-- by a separator (often a comma).
newtype Record s =
  Record {
    _fields :: NonEmptyVector (Spaced (Field s))
  }
  deriving (Eq, Ord, Show, Semigroup, Generic)

instance NFData s => NFData (Record s)

-- | A 'Record' is isomorphic to a 'NonEmpty' list of 'SpacedField's
recordSpacedFieldsIso :: Iso (Record s) (Record a) (NonEmptyVector (Spaced (Field s))) (NonEmptyVector (Spaced (Field a)))
recordSpacedFieldsIso = iso _fields Record
{-# INLINE recordSpacedFieldsIso #-}

-- | Classy lenses for 'Record'
class HasRecord s t a b | s -> a, t -> b where
  record :: Lens s t (Record a) (Record b)
  spacedFields :: Lens s t (NonEmptyVector (Spaced (Field a))) (NonEmptyVector (Spaced (Field b)))
  {-# INLINE spacedFields #-}
  spacedFields = record . spacedFields

instance HasRecord (Record a) (Record b) a b where
  record = id
  {-# INLINE record #-}
  spacedFields = recordSpacedFieldsIso
  {-# INLINE spacedFields #-}

instance HasFields (Record a) (Record b) a b where
  fields = spacedFields . traverse . spacedValue

instance Functor Record where
  fmap f = Record . fmap (fmap (fmap f)) . _fields

instance Foldable Record where
  foldMap f = foldMap (foldMap (foldMap f)) . _fields

instance Traversable Record where
  traverse f = fmap Record . traverse (traverse (traverse f)) . _fields

-- | Build an empty record.
--
-- According to RFC 4180, a record must have at least one field.
-- But a field can be the empty string. So this is the closest we can get to
-- an empty record.
--
-- Note that this does not make 'Record' a 'Monoid'. It is not a lawful unit
-- for the 'Semigroup' operation.
emptyRecord :: Monoid s => Record s
emptyRecord = singleField (Unquoted mempty)

-- | Build a 'Record' with just one 'Field'
singleField :: Field s -> Record s
singleField = Record . pure . pure

-- | Build a 'Record' given a 'NonEmpty' list of its fields
recordNel :: NonEmpty (SpacedField s) -> Record s
recordNel = Record . V.fromNel

-- | A collection of records, separated by newlines.
data Records s =
  EmptyRecords
  | Records (Record s) (Vector (Newline, Record s))
    deriving (Eq, Ord, Show, Generic)

instance NFData s => NFData (Records s)

-- | Prism for an empty 'Records'
_EmptyRecords :: Prism' (Records s) ()
_EmptyRecords =
  prism' (const EmptyRecords) $ \r ->
    case r of
      EmptyRecords -> Just ()
      Records _ _  -> Nothing

-- | Prism for a non-empty 'Records'
_NonEmptyRecords :: Prism (Records s) (Records t) (Record s, Vector (Newline, Record s)) (Record t, Vector (Newline, Record t))
_NonEmptyRecords =
  prism (uncurry Records) $ \r ->
    case r of
      EmptyRecords -> Left EmptyRecords
      Records a as -> Right (a,as)

-- | Classy lenses for 'Records'
class HasRecords c s | c -> s where
  records :: Lens' c (Records s)
  traverseRecords :: Traversal' c (Record s)
  traverseRecords = records . _NonEmptyRecords . beside id (traverse . _2)
  {-# INLINE traverseRecords #-}
  traverseNewlines :: Traversal' c Newline
  traverseNewlines = records . _NonEmptyRecords . _2 . traverse . _1

instance HasRecords (Records s) s where
  records = id
  {-# INLINE records #-}

instance Functor Records where
  fmap f rs = case rs of
    EmptyRecords -> EmptyRecords
    Records a as -> Records (fmap f a) (fmap (fmap (fmap f)) as)

instance Foldable Records where
  foldMap f rs = case rs of
    EmptyRecords -> mempty
    Records a as -> foldMap f a `mappend` foldMap (foldMap (foldMap f)) as

instance Traversable Records where
  traverse f rs = case rs of
    EmptyRecords -> pure EmptyRecords
    Records a as -> Records <$> traverse f a <*> traverse (traverse (traverse f)) as

-- | Convenience constructor for 'Records'.
--
-- This puts the same newline between all the records.
mkRecords :: Newline -> NonEmpty (Record s) -> Records s
mkRecords n (r:|rs) = Records r (V.fromList (fmap (n,) rs))

-- | A record collection conaining one record
singleRecord :: Record s -> Records s
singleRecord s = Records s V.empty

-- | Collect the list of 'Record's from anything that 'HasRecords'
recordList :: HasRecords c s => c -> [Record s]
recordList = toListOf traverseRecords
