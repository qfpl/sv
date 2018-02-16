{-# LANGUAGE DeriveGeneric #-}

module Data.Vector.NonEmpty where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Semigroup ((<>)))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Traversable (Traversable1 (traverse1))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data NonEmptyVector a =
  NonEmptyVector a (Vector a)
  deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (NonEmptyVector a) where

fromNel :: NonEmpty a -> NonEmptyVector a
fromNel (a :| as) = NonEmptyVector a (V.fromList as)

toNel :: NonEmptyVector a -> NonEmpty a
toNel (NonEmptyVector a as) = a :| V.toList as

instance Functor NonEmptyVector where
  fmap f (NonEmptyVector a as) = NonEmptyVector (f a) (fmap f as)

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
