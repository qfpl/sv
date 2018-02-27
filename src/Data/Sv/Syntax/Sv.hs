{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Sv.Syntax.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This file defines a datatype for a complete Sv document.
The datatype preserves information such as whitespace so that the original
text can be recovered.

In the usual workflow, this type is only an intermediate stage between
parsing and decoding.
-}

module Data.Sv.Syntax.Sv (
  Sv (Sv, _separatorSv, _maybeHeader, _records, _finalNewlines)
  , HasSv (sv, maybeHeader, traverseHeader, finalNewlines)
  , HasRecords (records, traverseNewlines, traverseRecords)
  , mkSv
  , emptySv
  , recordList
  , Header (Header, _headerRecord)
  , HasHeader (header, headerRecord, headerNewline)
  , noHeader
  , mkHeader
  , Headedness (Unheaded, Headed)
  , HasHeadedness (headedness)
  , getHeadedness
  , Separator
  , HasSeparator (separator)
  , comma
  , pipe
  , tab
) where

import Control.DeepSeq    (NFData)
import Control.Lens       (Lens', Traversal')
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Monoid        ((<>))
import Data.Traversable   (Traversable (traverse))
import GHC.Generics       (Generic)

import Data.Sv.Syntax.Field  (HasFields (fields))
import Data.Sv.Syntax.Record (Record, Records (EmptyRecords), HasRecord (record), HasRecords (records, traverseNewlines, traverseRecords), recordList)
import Text.Newline       (Newline)

-- | 'Sv' is a whitespace-preserving data type for separated values.
--   Often the separator is a comma, but this type does not make that
--   assumption so that it can be used for pipe- or tab-separated values
--   as well.
data Sv s =
  Sv {
    _separatorSv :: Separator
  , _maybeHeader :: Maybe (Header s)
  , _records :: Records s
  , _finalNewlines :: [Newline]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData s => NFData (Sv s)

-- | Classy lenses for 'Sv'
class (HasRecords c s, HasSeparator c) => HasSv c s | c -> s where
  sv :: Lens' c (Sv s)
  maybeHeader :: Lens' c (Maybe (Header s))
  {-# INLINE maybeHeader #-}
  traverseHeader :: Traversal' c (Header s)
  {-# INLINE traverseHeader #-}
  finalNewlines :: Lens' c [Newline]
  {-# INLINE finalNewlines #-}
  maybeHeader = sv . maybeHeader
  traverseHeader = maybeHeader . traverse
  finalNewlines = sv . finalNewlines

instance HasSeparator (Sv s) where
  separator f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv y x2 x3 x4) (f x1)
  {-# INLINE separator #-}

instance HasRecords (Sv s) s where
  records f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 x2 y x4) (f x3)
  {-# INLINE records #-}

instance HasSv (Sv s) s where
  {-# INLINE maybeHeader #-}
  {-# INLINE finalNewlines #-}
  sv = id
  maybeHeader f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 y x3 x4) (f x2)
  finalNewlines f (Sv x1 x2 x3 x4) =
    fmap (Sv x1 x2 x3) (f x4)

-- | Convenience constructor for Sv
mkSv :: Separator -> Maybe (Header s) -> [Newline] -> Records s -> Sv s
mkSv c h ns rs = Sv c h rs ns

-- | An empty Sv
emptySv :: Separator -> Sv s
emptySv c = Sv c Nothing EmptyRecords []

instance Functor Sv where
  fmap f (Sv s h rs e) = Sv s (fmap (fmap f) h) (fmap f rs) e

instance Foldable Sv where
  foldMap f (Sv _ h rs _) = foldMap (foldMap f) h <> foldMap f rs

instance Traversable Sv where
  traverse f (Sv s h rs e) = Sv s <$> traverse (traverse f) h <*> traverse f rs <*> pure e

-- | Determine the 'Headedness' of an 'Sv'
getHeadedness :: Sv s -> Headedness
getHeadedness = maybe Unheaded (const Headed) . _maybeHeader

-- | A 'Header' is present in many CSV documents, usually listing the names
-- of the columns. We keep this separate from the regular records.
data Header s =
  Header {
    _headerRecord :: Record s
  , _headerNewline :: Newline
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData s => NFData (Header s)

-- | Classy lenses for 'Header'
class HasHeader c s | c -> s where
  header :: Lens' c (Header s)
  headerNewline :: Lens' c Newline
  {-# INLINE headerNewline #-}
  headerRecord :: Lens' c (Record s)
  {-# INLINE headerRecord #-}
  headerNewline = header . headerNewline
  headerRecord = header . headerRecord

instance HasHeader (Header s) s where
  {-# INLINE headerNewline #-}
  {-# INLINE headerRecord #-}
  header = id
  headerNewline f (Header x1 x2)
    = fmap (Header x1) (f x2)
  headerRecord f (Header x1 x2)
    = fmap (\y -> Header y x2) (f x1)

instance HasRecord (Header s) s where
  record = headerRecord
  {-# INLINE record #-}

instance HasFields (Header s) s where
  fields = headerRecord . fields

-- | Used to build 'Sv's that don't have a header
noHeader :: Maybe (Header s)
noHeader = Nothing

-- | Convenience constructor for 'Header', usually when you're building 'Sv's
mkHeader :: Record s -> Newline -> Maybe (Header s)
mkHeader r n = Just (Header r n)

-- | Does the 'Sv' have a 'Header' or not? A header is a row at the beginning
-- of a file which contains the string names of each of the columns.
--
-- If a header is present, it must not be decoded with the rest of the data.
data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

-- | Classy lens for 'Headedness'
class HasHeadedness c where
  headedness :: Lens' c Headedness

instance HasHeadedness Headedness where
  headedness = id

-- | By what are your values separated? The answer is often 'comma', but not always.
--
-- A 'Separator' is just a 'Char'. It could be a sum type instead, since it
-- will usually be comma or pipe, but our preference has been to be open here
-- so that you can use whatever you'd like. There are test cases, for example,
-- ensuring that you're free to use null-byte separated values if you so desire.
type Separator = Char

-- | Classy lens for 'Separator'
class HasSeparator c where
  separator :: Lens' c Separator

instance HasSeparator Char where
  separator = id
  {-# INLINE separator #-}

-- | The venerable comma separator. Used for CSV documents.
comma :: Separator
comma = ','

-- | The pipe separator. Used for PSV documents.
pipe :: Separator
pipe = '|'

-- | Tab is a separator too - why not?
tab :: Separator
tab = '\t'
