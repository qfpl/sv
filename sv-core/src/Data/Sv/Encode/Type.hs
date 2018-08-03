{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Sv.Encode.Type
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

The core type for encoding
-}

module Data.Sv.Encode.Type (
  Encode (Encode, getEncode)
, NameEncode (..)
) where

import Control.Applicative (liftA2)
import Control.Monad.Writer (Writer)
import Data.Bifoldable (bifoldMap)
import Data.ByteString.Builder (Builder)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Compose (ComposeFC (..))
import Data.Functor.Contravariant.Divisible (Divisible (divide, conquer), Decidable (choose, lose))
import Data.Semigroup (Semigroup ((<>)))
import Data.Sequence (Seq)
import Data.Void (absurd)

import Data.Sv.Encode.Options

-- | An 'Encode' converts its argument into one or more textual fields, to be
-- written out as CSV.
--
-- It is 'Semigroup', 'Monoid', 'Contravariant', 'Divisible', and 'Decidable',
-- allowing for composition of these values to build bigger 'Encode's
-- from smaller ones.
newtype Encode a =
  Encode { getEncode :: EncodeOptions -> a -> Seq Builder }
  deriving (Semigroup, Monoid)

instance Contravariant Encode where
  contramap f (Encode g) = Encode $ fmap (. f) g

instance Divisible Encode where
  conquer = Encode mempty
  divide f (Encode x) (Encode y) =
    Encode $ \e a -> bifoldMap (x e) (y e) (f a)

instance Decidable Encode where
  lose f = Encode (const (absurd . f))
  choose f (Encode x) (Encode y) =
    Encode $ \e a -> either (x e) (y e) (f a)

-- | A 'NameEncode' is an 'Encode' with an attached column name.
--
-- It is 'Semigroup', 'Monoid', 'Contravariant', and 'Divisible', allowing
-- for composition of these values to build bigger 'NameEncode's
-- from smaller ones.
--
-- Notably, 'NameEncode' is not 'Decidable', since taking the sum of column
-- names does not make sense.
newtype NameEncode a =
  NameEncode { unNamedE :: ComposeFC (Writer (Seq Builder)) Encode a}
  deriving (Contravariant, Divisible) -- intentionally not Decidable

instance Semigroup (NameEncode a) where
  NameEncode (ComposeFC a) <> NameEncode (ComposeFC b) =
    NameEncode (ComposeFC (liftA2 (<>) a b))

instance Monoid (NameEncode a) where
  mappend = (<>)
  mempty = NameEncode (ComposeFC (pure mempty))
