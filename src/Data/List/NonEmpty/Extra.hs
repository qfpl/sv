{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.List.NonEmpty.Extra where

import Control.Lens ((^.), Prism', prism', preview, review)
import Data.Bifunctor (first)
import Data.ByteString  (ByteString)
import Data.ByteString1 (ByteString1, fromByteString, toByteString)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Text as T
import Data.Text1 (Text1, _Text1)

import Data.Semigroup.Foldable.Extra (toNonEmpty)
import Data.Csv.Record.Simple (Record (Record), fields, singleton)
import Data.Csv.Record.NonEmpty (NonEmptyRecord (SingleFieldNER, MultiFieldNER))
import Data.List.AtLeastTwo (_AtLeastTwo)

-- It is really helpful that the fundep is bidirectional,
-- but that makes nearly any instance outside this file an orphan.
class AsNonEmpty nonempty empty | nonempty -> empty, empty -> nonempty where
  _NonEmpty :: Prism' empty nonempty

instance AsNonEmpty (NonEmpty a) [a] where
  _NonEmpty = prism' toList nonEmpty

instance AsNonEmpty Text1 T.Text where
  _NonEmpty = _Text1

instance AsNonEmpty ByteString1 ByteString where
  _NonEmpty = prism' toByteString fromByteString

instance AsNonEmpty s1 s2 => AsNonEmpty (NonEmptyRecord s1 s2) (Record s2) where
  _NonEmpty = prism'
    (\ner -> case ner of
      SingleFieldNER r -> singleton (first (review _NonEmpty) r)
      MultiFieldNER zs -> Record (toNonEmpty zs))
    (\r -> MultiFieldNER <$> preview _AtLeastTwo (r ^. fields))
