{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | This file defines a datatype for a complete Sv document.
-- The datatype preserves information so that the original Sv
-- text can be recovered.
module Data.Sv.Sv (
  Sv (Sv, _separator, _maybeHeader, _records, _finalNewlines)
  , HasSv (sv, separator, maybeHeader, finalNewlines)
  , HasRecords (records, theRecords)
  , mkSv
  , mkSv'
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

-- | 'Sv' is a whitespace-preserving data type for separated values.
--   Often the separator is a comma, but this type does not make that
--   assumption so that it can be used for pipe- or tab-separated values
--   as well.
data Sv s =
  Sv {
    _separator :: Separator
  , _maybeHeader :: Maybe (Header s)
  , _records :: Records s
  , _finalNewlines :: [Newline]
  }
  deriving (Eq, Ord, Show)

-- | Classy lenses for 'Sv'
class HasRecords c s => HasSv c s | c -> s where
  sv :: Lens' c (Sv s)
  separator :: Lens' c Separator
  {-# INLINE separator #-}
  maybeHeader :: Lens' c (Maybe (Header s))
  {-# INLINE maybeHeader #-}
  finalNewlines :: Lens' c [Newline]
  {-# INLINE finalNewlines #-}
  separator = sv . separator
  maybeHeader = sv . maybeHeader
  finalNewlines = sv . finalNewlines

instance HasRecords (Sv s) s where
  records f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 x2 y x4) (f x3)
  {-# INLINE records #-}

instance HasSv (Sv s) s where
  {-# INLINE separator #-}
  {-# INLINE maybeHeader #-}
  {-# INLINE finalNewlines #-}
  sv = id
  separator f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv y x2 x3 x4) (f x1)
  maybeHeader f (Sv x1 x2 x3 x4) =
    fmap (\y -> Sv x1 y x3 x4) (f x2)
  finalNewlines f (Sv x1 x2 x3 x4) =
    fmap (Sv x1 x2 x3) (f x4)

-- | Convenience constructor for Sv
mkSv :: Separator -> Maybe (Header s) -> [Newline] -> Records s -> Sv s
mkSv c h ns rs = Sv c h rs ns

-- | Convenience constructor for Sv
mkSv' :: Separator -> Maybe (Header s) -> [Newline] -> Maybe (Pesarated1 Newline (Record s)) -> Sv s
mkSv' c h ns = mkSv c h ns . Records

-- | An empty Sv
empty :: Separator -> Sv s
empty c = Sv c Nothing emptyRecords []

instance Functor Sv where
  fmap f (Sv s h rs e) = Sv s (fmap (fmap f) h) (fmap f rs) e

instance Foldable Sv where
  foldMap f (Sv _ h rs _) = foldMap (foldMap f) h <> foldMap f rs

instance Traversable Sv where
  traverse f (Sv s h rs e) = Sv s <$> traverse (traverse f) h <*> traverse f rs <*> pure e

-- | A 'Separator' is just a 'Char'. It could be a sum type instead, since it
-- will usually be comma or pipe, but our preference has been to be open here
-- so that you can use whatever you'd like.
type Separator = Char

-- | The venerable comma separator. Used for CSV documents.
comma :: Separator
comma = ','

-- | The pipe separator. Used for PSV documents.
pipe :: Separator
pipe = '|'

-- | Tab is a separator too - why not?
tab :: Separator
tab = '\t'

-- | A 'Header' is present in many CSV documents, usually listing the names
-- of the columns. We keep this separate from the regular records.
data Header s =
  Header {
    _headerRecord :: Record s
  , _headerNewline :: Newline
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- | Used to build 'Sv's that don't have a header
noHeader :: Maybe (Header s)
noHeader = Nothing

-- | Convenience constructor for 'Header', usually when you're building 'Sv's
mkHeader :: Record s -> Newline -> Maybe (Header s)
mkHeader r n = Just (Header r n)

-- | Does the 'Sv' have a 'Header' or not?
data Headedness =
  Unheaded | Headed
  deriving (Eq, Ord, Show)

-- | Determine the 'Headedness' of an 'Sv'
headedness :: Sv s -> Headedness
headedness = maybe Unheaded (const Headed) . _maybeHeader
