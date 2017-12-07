{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | This file defines a datatype for a complete CSV document.
-- The datatype preserves information so that the original CSV
-- text can be recovered.
module Data.Sv.Sv (
  Csv (Csv, _separator, _maybeHeader, _records, _finalNewlines)
  , HasCsv (csv, separator, maybeHeader, finalNewlines)
  , HasRecords (records, theRecords)
  , mkCsv
  , mkCsv'
  , empty
  , recordList
  , Header (Header, _headerRecord)
  , HasHeader (header, headerRecord)
  , noHeader
  , mkHeader
  , Headedness (Unheaded, Headed)
  , headedness
  , Separator
  , comma
  , pipe
  , tab
) where

import Control.Lens       (Lens')
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.Monoid        ((<>))
import Data.Separated     (Pesarated1)
import Data.Traversable   (Traversable (traverse))

import Data.Sv.Record     (Record, Records (Records), HasRecords (records, theRecords), emptyRecords, recordList)
import Text.Newline       (Newline)

-- | 'Csv' is a whitespace-preserving data type for separated values.
--   Often the separator is a comma, but this type does not make that
--   assumption so that it can be used for pipe- or tab-separated values
--   as well.
data Csv s =
  Csv {
    _separator :: Separator
  , _maybeHeader :: Maybe (Header s)
  , _records :: Records s
  , _finalNewlines :: [Newline]
  }
  deriving (Eq, Ord, Show)

class HasRecords c s => HasCsv c s | c -> s where
  csv :: Lens' c (Csv s)
  separator :: Lens' c Separator
  {-# INLINE separator #-}
  maybeHeader :: Lens' c (Maybe (Header s))
  {-# INLINE maybeHeader #-}
  finalNewlines :: Lens' c [Newline]
  {-# INLINE finalNewlines #-}
  separator = csv . separator
  maybeHeader = csv . maybeHeader
  finalNewlines = csv . finalNewlines

instance HasRecords (Csv s) s where
  records f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv x1 x2 y x4) (f x3)
  {-# INLINE records #-}

instance HasCsv (Csv s) s where
  {-# INLINE separator #-}
  {-# INLINE maybeHeader #-}
  {-# INLINE finalNewlines #-}
  csv = id
  separator f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv y x2 x3 x4) (f x1)
  maybeHeader f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv x1 y x3 x4) (f x2)
  finalNewlines f (Csv x1 x2 x3 x4) =
    fmap (Csv x1 x2 x3) (f x4)

-- | Convenience constructor for CSV
mkCsv :: Separator -> Maybe (Header s) -> [Newline] -> Records s -> Csv s
mkCsv c h ns rs = Csv c h rs ns

-- | Convenience constructor for CSV
mkCsv' :: Separator -> Maybe (Header s) -> [Newline] -> Maybe (Pesarated1 Newline (Record s)) -> Csv s
mkCsv' c h ns = mkCsv c h ns . Records

empty :: Separator -> Csv s
empty c = Csv c Nothing emptyRecords []

instance Functor Csv where
  fmap f (Csv s h rs e) = Csv s (fmap (fmap f) h) (fmap f rs) e

instance Foldable Csv where
  foldMap f (Csv _ h rs _) = foldMap (foldMap f) h <> foldMap f rs

instance Traversable Csv where
  traverse f (Csv s h rs e) = Csv s <$> traverse (traverse f) h <*> traverse f rs <*> pure e

type Separator = Char

comma :: Separator
comma = ','

pipe :: Separator
pipe = '|'

tab :: Separator
tab = '\t'

data Header s =
  Header {
    _headerRecord :: Record s
  , _headerNewline :: Newline
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

noHeader :: Maybe (Header s)
noHeader = Nothing

mkHeader :: Record s -> Newline -> Maybe (Header s)
mkHeader r n = Just (Header r n)

data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

headedness :: Csv s -> Headedness
headedness = maybe Unheaded (const Headed) . _maybeHeader
