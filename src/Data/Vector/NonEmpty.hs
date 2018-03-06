{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Data.Vector.NonEmpty
Copyright   : (C) CSIRO 2017-2018
License     : BSD3
Maintainer  : George Wilson <george.wilson@data61.csiro.au>
Stability   : experimental
Portability : non-portable
-}

module Data.Vector.NonEmpty (
  NonEmptyVector (NonEmptyVector)
, fromNel
, toNel
, headNev
, tailNev
) where

import Control.DeepSeq (NFData)
import Control.Lens (Lens', lens)
import Data.Functor.Apply (Apply((<.>)))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Traversable (Traversable1 (traverse1))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

-- | A non-empty value of 'Vector'
data NonEmptyVector a =
  NonEmptyVector a (Vector a)
  deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (NonEmptyVector a) where

-- | Convert a 'NonEmpty' list to a 'NonEmptyVector'
fromNel :: NonEmpty a -> NonEmptyVector a
fromNel (a :| as) = NonEmptyVector a (V.fromList as)

-- | Convert a 'NonEmptyVector' to a 'NonEmpty' list
toNel :: NonEmptyVector a -> NonEmpty a
toNel (NonEmptyVector a as) = a :| V.toList as

instance Functor NonEmptyVector where
  fmap f (NonEmptyVector a as) = NonEmptyVector (f a) (fmap f as)

instance Apply NonEmptyVector where
  (<.>) = (<*>)

instance Applicative NonEmptyVector where
  pure a = NonEmptyVector a V.empty
  ff <*> fa = fromNel (toNel ff <*> toNel fa)

instance Foldable NonEmptyVector where
  foldMap f (NonEmptyVector a as) = f a `mappend` foldMap f as

instance Foldable1 NonEmptyVector where
  foldMap1 f (NonEmptyVector a as) = foldMap1 f (a :| toList as)

instance Traversable NonEmptyVector where
  traverse f (NonEmptyVector a as) = NonEmptyVector <$> f a <*> traverse f as

instance Traversable1 NonEmptyVector where
  traverse1 f = fmap fromNel . traverse1 f . toNel

instance Semigroup (NonEmptyVector a) where
  NonEmptyVector a as <> NonEmptyVector b bs =
    NonEmptyVector a (V.concat [as, V.singleton b, bs])

-- | Get or set the head of a 'NonEmptyVector'
headNev :: Lens' (NonEmptyVector a) a
headNev = lens (\(NonEmptyVector h _) -> h) (\(NonEmptyVector _ t) h -> NonEmptyVector h t)

-- | Get or set the head of a 'NonEmptyVector'
tailNev :: Lens' (NonEmptyVector a) (Vector a)
tailNev = lens (\(NonEmptyVector _ t) -> t) (\(NonEmptyVector h _) t -> NonEmptyVector h t)
