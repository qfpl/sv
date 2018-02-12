{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Sv.Encode.Type
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable

The core types for encoding
-}

module Data.Sv.Encode.Type (
  Encode (Encode, getEncode)
) where

import Data.Bifoldable (bifoldMap)
import qualified Data.ByteString.Builder as BS
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Divisible (divide, conquer), Decidable (choose, lose))
import Data.Semigroup (Semigroup)
import Data.Sequence (Seq)
import Data.Void (absurd)

import Data.Sv.Encode.Options

newtype Encode a =
  Encode { getEncode :: EncodeOptions -> a -> Seq BS.Builder }
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
