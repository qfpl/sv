{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Csv.Headed (
  Header (Header, _record)
  , headerRecord
  , HasHeader (header)
  , Headed (Headed, _header, _data)
  , makeHeaded
) where

import Control.Lens    (Iso, Lens', iso, lens)

import Data.Csv.Csv    (Csv, unconsRecord)
import Data.Csv.Record (HasRecord (record), Record)
import Data.List.NonEmpty.Extra (AsNonEmpty)
import Text.Newline    (Newline)

newtype Header s =
  Header { _record :: Record s }
  deriving (Eq, Ord, Show)

headerRecord :: Iso (Header s) (Header t) (Record s) (Record t)
headerRecord = iso _record Header

class HasRecord s t => HasHeader s t | s -> t where
  header :: Lens' s (Header t)

instance HasRecord (Header s) s where
  record = headerRecord

instance HasHeader (Header s) s where
  header = id

data Headed s1 s2 =
  Headed {
    _header :: Header s2
  , _data :: Maybe (Newline, Csv s1 s2)
  }
  deriving (Eq, Ord, Show)

instance HasRecord (Headed s1 s2) s2 where
  record = header . record

instance HasHeader (Headed s1 s2) s2 where
  header = lens _header (\(Headed _ d) h -> Headed h d)

makeHeaded :: AsNonEmpty s1 s2 => Csv s1 s2 -> Maybe (Headed s1 s2)
makeHeaded c =
  fmap make (unconsRecord c)
    where
    make ((r, mn), c') = Headed (Header r) (fmap (,c') mn)

