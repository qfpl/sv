{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  , Records (Records, _theRecords)
  , HasRecords (records, theRecords, traverseRecords)
  , emptyRecords
  , singleRecord
  , recordList
) where

import Control.DeepSeq    (NFData)
import Control.Lens       (Lens', Iso, Traversal', iso, view)
import Data.Foldable      (Foldable (foldMap), toList)
import Data.Functor       (Functor (fmap))
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup     (Semigroup)
import Data.Separated     (Pesarated1 (Pesarated1), Separated1 (Separated1), Separated (Separated))
import Data.Traversable   (Traversable (traverse))
import GHC.Generics       (Generic)

import Data.Sv.Syntax.Field      (Field (Unquoted), HasFields (fields))
import Text.Newline       (Newline)
import Text.Space         (Spaced, spacedValue)


-- | A @Record@ is a non-empty collection of Fields, implicitly separated
-- by a separator (often a comma).
newtype Record s =
  Record {
    _fields :: NonEmpty (Spaced (Field s))
  }
  deriving (Eq, Ord, Show, Semigroup, Generic)

instance NFData s => NFData (Record s)

-- | A 'Record' is isomorphic to a 'NonEmpty' list of 'SpacedField's
recordSpacedFieldsIso :: Iso (Record s) (Record a) (NonEmpty (Spaced (Field s))) (NonEmpty (Spaced (Field a)))
recordSpacedFieldsIso = iso _fields Record
{-# INLINE recordSpacedFieldsIso #-}

-- | Classy lenses for 'Record'
class HasRecord s t | s -> t where
  record :: Lens' s (Record t)
  spacedFields :: Lens' s (NonEmpty (Spaced (Field t)))
  {-# INLINE spacedFields #-}
  spacedFields = record . spacedFields

instance HasRecord (Record s) s where
  record = id
  {-# INLINE record #-}
  spacedFields = recordSpacedFieldsIso
  {-# INLINE spacedFields #-}

instance HasFields (Record s) s where
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

-- | A collection of records, separated by newlines.
newtype Records s =
  Records { _theRecords :: Maybe (Pesarated1 Newline (Record s)) }
  deriving (Eq, Ord, Show, Generic)

instance NFData s => NFData (Records s)

deriving instance Generic (Separated1 a b)
deriving instance Generic (Pesarated1 a b)
deriving instance Generic (Separated  a b)

instance (NFData a, NFData b) => NFData (Separated  a b)
instance (NFData a, NFData b) => NFData (Separated1 a b)
instance (NFData a, NFData b) => NFData (Pesarated1 a b)

-- | Classy lenses for 'Records'
class HasRecords c s | c -> s where
  records :: Lens' c (Records s)
  theRecords :: Lens' c (Maybe (Pesarated1 Newline (Record s)))
  {-# INLINE theRecords #-}
  theRecords = records . theRecords
  traverseRecords :: Traversal' c (Record s)
  traverseRecords = theRecords . traverse . traverse
  {-# INLINE traverseRecords #-}

instance HasRecords (Records s) s where
  {-# INLINE theRecords #-}
  records = id
  theRecords = iso _theRecords Records

instance Functor Records where
  fmap f = Records . fmap (fmap (fmap f)) . view theRecords

instance Foldable Records where
  foldMap f = foldMap (foldMap (foldMap f)) . view theRecords

instance Traversable Records where
  traverse f = fmap Records . traverse (traverse (traverse f)) . view theRecords

-- | A collection of records containing no records.
emptyRecords :: Records s
emptyRecords = Records Nothing

-- | A record collection conaining one record
singleRecord :: Record s -> Records s
singleRecord s = Records (Just (Pesarated1 (Separated1 s mempty)))

-- | Collect the list of 'Record's from anything that 'HasRecords'
recordList :: HasRecords c s => c -> [Record s]
recordList = foldMap toList . view theRecords
