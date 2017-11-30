{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.List.AtLeastTwo (
  AtLeastTwo (AtLeastTwo, _car, _cdr)
, mkAtLeastTwo
, HasAtLeastTwo (atLeastTwo, car, cdr)
, AsAtLeastTwo (_AtLeastTwo)
) where

import Control.Lens (Lens', _Wrapped, Prism', prism')
import Control.Lens.Tuple (Field1 (_1), Field2 (_2))
import Data.Foldable (Foldable (foldMap), toList)
import Data.Functor (Functor (fmap))
import Data.Functor.Apply ((<.>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Foldable.Extra (toNonEmpty)
import Data.Semigroup.Traversable (Traversable1 (traverse1))
import Data.Traversable (Traversable (traverse))

data AtLeastTwo a =
  AtLeastTwo {
    _car :: a
  , _cdr :: NonEmpty a
  }
  deriving (Eq, Ord, Show)

instance Functor AtLeastTwo where
  fmap f (AtLeastTwo a as) = AtLeastTwo (f a) (fmap f as)

-- TODO Not sure which if any Applicative belongs

instance Foldable AtLeastTwo where
  foldMap f (AtLeastTwo a as) = f a `mappend` foldMap f as

instance Traversable AtLeastTwo where
  traverse f (AtLeastTwo a as) = AtLeastTwo <$> f a <*> traverse f as

instance Foldable1 AtLeastTwo where
  foldMap1 f (AtLeastTwo a as) = f a <> foldMap1 f as

instance Traversable1 AtLeastTwo where
  traverse1 f (AtLeastTwo a as) = AtLeastTwo <$> f a <.> traverse1 f as

instance Field1 (AtLeastTwo a) (AtLeastTwo a) a a where
  _1 = car

instance Field2 (AtLeastTwo a) (AtLeastTwo a) a a where
  _2 = cadr

mkAtLeastTwo :: a -> a -> [a] -> AtLeastTwo a
mkAtLeastTwo x y ys = AtLeastTwo x (y:|ys)

class HasAtLeastTwo s a | s -> a where
  atLeastTwo :: Lens' s (AtLeastTwo a)
  car :: Lens' s a
  {-# INLINE car #-}
  cdr :: Lens' s (NonEmpty a)
  {-# INLINE cdr #-}
  cadr :: Lens' s a
  {-# INLINE cadr #-}
  car = atLeastTwo . car
  cdr = atLeastTwo . cdr
  cadr = atLeastTwo . cadr

instance HasAtLeastTwo (AtLeastTwo a) a where
  {-# INLINE car #-}
  {-# INLINE cdr #-}
  atLeastTwo = id
  car f (AtLeastTwo x y)
    = fmap (flip AtLeastTwo y) (f x)
  cdr f (AtLeastTwo x y)
    = fmap (AtLeastTwo x) (f y)
  cadr =
    cdr . _Wrapped . _1

class AsAtLeastTwo s a | s -> a where
  _AtLeastTwo :: Prism' s (AtLeastTwo a)

instance AsAtLeastTwo (AtLeastTwo a) a where
  _AtLeastTwo = id

instance AsAtLeastTwo [a] a where
  _AtLeastTwo = prism' toList $ \xs -> case xs of
    [] -> Nothing
    [_] -> Nothing
    x:y:ys -> Just (AtLeastTwo x (y:|ys))

instance AsAtLeastTwo (NonEmpty a) a where
  _AtLeastTwo = prism' toNonEmpty $ \xs -> case xs of
    _:|[] -> Nothing
    x:|y:ys -> Just (AtLeastTwo x (y:|ys))

