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
module Data.Csv.Csv (
  Csv (Csv, _csvSeparator, _csvHeader, _csvInitialRecords, _csvFinalRecord)
  , HasCsv (csv, csvFinalRecord, csvInitialRecords, csvSeparator)
  , mkCsv
  , mkCsv'
  , empty
  , unconsRecord
  , records
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

import Control.Lens       ((^.), Lens', review)
import Data.Bifoldable    (Bifoldable (bifoldMap))
import Data.Bifunctor     (Bifunctor (bimap), second)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable      (Foldable (foldMap))
import Data.Functor       (Functor (fmap), (<$>))
import Data.List.NonEmpty.Extra (AsNonEmpty (_NonEmpty))
import Data.Monoid        ((<>))
import Data.Separated     (Pesarated (Pesarated), Separated (Separated))
import Data.Traversable   (Traversable (traverse))

import Data.Csv.Record    (Record, Records (Records), HasRecords (theRecords), FinalRecord (FinalRecord), HasFinalRecord (maybeNer), noFinal, nonEmptyRecord)
import Text.Newline       (Newline)

-- | 'Csv' is a whitespace-preserving data type for separated values.
--   Often the separator is a comma, but this type does not make that
--   assumption so that it can be used for pipe- or tab-separated values
--   as well.
data Csv s1 s2 =
  Csv {
    _csvSeparator :: Separator
  , _csvHeader :: Maybe (Header s2)
  , _csvInitialRecords :: Records s2
  , _csvFinalRecord :: FinalRecord s1 s2
  }
  deriving (Eq, Ord, Show)

class HasCsv c s1 s2 | c -> s1 s2 where
  csv :: Lens' c (Csv s1 s2)
  csvSeparator :: Lens' c Separator
  {-# INLINE csvSeparator #-}
  csvHeader :: Lens' c (Maybe (Header s2))
  {-# INLINE csvHeader #-}
  csvInitialRecords :: Lens' c (Records s2)
  {-# INLINE csvInitialRecords #-}
  csvFinalRecord :: Lens' c (FinalRecord s1 s2)
  {-# INLINE csvFinalRecord #-}
  csvSeparator = csv . csvSeparator
  csvHeader = csv . csvHeader
  csvInitialRecords = csv . csvInitialRecords
  csvFinalRecord = csv . csvFinalRecord

instance HasCsv (Csv s1 s2) s1 s2 where
  {-# INLINE csvSeparator #-}
  {-# INLINE csvHeader #-}
  {-# INLINE csvInitialRecords #-}
  {-# INLINE csvFinalRecord #-}
  csv = id
  csvSeparator f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv y x2 x3 x4) (f x1)
  csvHeader f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv x1 y x3 x4) (f x2)
  csvInitialRecords f (Csv x1 x2 x3 x4) =
    fmap (\y -> Csv x1 x2 y x4) (f x3)
  csvFinalRecord f (Csv x1 x2 x3 x4) =
    fmap (Csv x1 x2 x3) (f x4)

-- | Convenience constructor for CSV
mkCsv :: Separator -> Maybe (Header s2) -> FinalRecord s1 s2 -> Records s2 -> Csv s1 s2
mkCsv c h ns rs = Csv c h rs ns

-- | Convenience constructor for CSV
mkCsv' :: Separator -> Maybe (Header s2) -> FinalRecord s1 s2 -> Pesarated Newline (Record s2) -> Csv s1 s2
mkCsv' c h ns = mkCsv c h ns . Records

empty :: Separator -> Csv s1 s2
empty c = Csv c noHeader mempty noFinal

instance Functor (Csv s) where
  fmap = second

instance Foldable (Csv s) where
  foldMap = bifoldMap (const mempty)

instance Traversable (Csv s) where
  traverse = bitraverse pure

instance Bifunctor Csv where
  bimap f g (Csv s h rs e) = Csv s (fmap (fmap g) h) (fmap g rs) (bimap f g e)

instance Bifoldable Csv where
  bifoldMap f g (Csv _ h rs e) = foldMap (foldMap g) h <> foldMap g rs <> bifoldMap f g e

instance Bitraversable Csv where
  bitraverse f g (Csv s h rs e) = Csv s <$> traverse (traverse g) h <*> traverse g rs <*> bitraverse f g e

unconsRecord :: AsNonEmpty s1 s2 => Csv s1 s2 -> Maybe ((Record s2, Maybe Newline), Csv s1 s2)
unconsRecord (Csv sep h initial final) =
  case initial ^. theRecords of
    Pesarated (Separated []) -> case final ^. maybeNer of
      Nothing -> Nothing
      Just r -> Just ((review nonEmptyRecord r, Nothing), empty sep)
    Pesarated (Separated ((r,n):rs)) -> Just ((r,Just n), Csv sep h (Records (Pesarated (Separated rs))) final)

-- TODO this can be a lot faster. Should it be list? Probably not.
records :: AsNonEmpty s1 s2 => Csv s1 s2 -> [Record s2]
records (Csv _ _ (Records initial) (FinalRecord final)) =
  foldr (:) final' initial
  where final' = foldMap (pure . review _NonEmpty) final

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

headedness :: Csv s1 s2 -> Headedness
headedness = maybe Unheaded (const Headed) . _csvHeader
