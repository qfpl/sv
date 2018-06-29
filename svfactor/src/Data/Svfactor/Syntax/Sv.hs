{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Svfactor.Syntax.Sv
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

This file defines a datatype for a complete Sv document.
The datatype preserves information such as whitespace so that the original
text can be recovered.

You can program against it using the provided functions and optics.
For an example of this see
<https://github.com/qfpl/svfactor/blob/master/examples/src/Data/Svfactor/Example/Requote.hs Requote.hs>
-}

module Data.Svfactor.Syntax.Sv (
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

import Control.DeepSeq (NFData)
import Control.Lens (Lens, Lens', Traversal')
import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap), (<$>))
import Data.Monoid ((<>))
import Data.Traversable (Traversable (traverse))
import GHC.Generics (Generic)

import Data.Svfactor.Structure.Headedness (Headedness (Headed, Unheaded), HasHeadedness (headedness))
import Data.Svfactor.Syntax.Field (HasFields (fields))
import Data.Svfactor.Syntax.Record (Record, Records (EmptyRecords), HasRecord (record), HasRecords (records, traverseNewlines, traverseRecords), recordList)
import Data.Svfactor.Text.Newline (Newline)
import Data.Svfactor.Text.Separator (Separator, HasSeparator (separator), comma, pipe, tab)

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

instance HasRecords (Sv s) s where
  records f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 x2 y x4) (f x3)
  {-# INLINE records #-}

instance HasSv (Sv s) s where
  sv = id
  {-# INLINE sv #-}
  maybeHeader f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 y x3 x4) (f x2)
  {-# INLINE maybeHeader #-}
  finalNewlines f (Sv x1 x2 x3 x4) =
    fmap (Sv x1 x2 x3) (f x4)
  {-# INLINE finalNewlines #-}

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
class HasHeader s t a b | s -> a, t -> b, s b -> t, t a -> s where
  header :: Lens s t (Header a) (Header b)
  headerNewline :: (s ~ t) => Lens s t Newline Newline
  {-# INLINE headerNewline #-}
  headerRecord :: Lens s t (Record a) (Record b)
  {-# INLINE headerRecord #-}
  default headerNewline :: (a ~ b) => Lens s t Newline Newline
  headerNewline = header . headerNewline
  headerRecord = header . headerRecord

instance HasHeader (Header a) (Header b) a b where
  header = id
  {-# INLINE header #-}
  headerNewline f (Header x1 x2)
    = fmap (Header x1) (f x2)
  {-# INLINE headerNewline #-}
  headerRecord f (Header x1 x2)
    = fmap (\y -> Header y x2) (f x1)
  {-# INLINE headerRecord #-}

instance HasRecord (Header a) (Header b) a b where
  record = headerRecord
  {-# INLINE record #-}

instance HasFields (Header a) (Header b) a b where
  fields = headerRecord . fields

-- | Used to build 'Sv's that don't have a header
noHeader :: Maybe (Header s)
noHeader = Nothing

-- | Convenience constructor for 'Header', usually when you're building 'Sv's
mkHeader :: Record s -> Newline -> Maybe (Header s)
mkHeader r n = Just (Header r n)

instance HasSeparator (Sv s) where
  separator f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv y x2 x3 x4) (f x1)
  {-# INLINE separator #-}
