module Data.List.AtLeastTwo where

import Prelude hiding (head)
import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap))
import Data.Functor.Apply ((<.>))
import Data.List.NonEmpty (NonEmpty, head)
import Data.Semigroup ((<>))
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Semigroup.Traversable (Traversable1 (traverse1))
import Data.Traversable (Traversable (traverse))

data AtLeastTwo a =
  AtLeastTwo {
    car :: a
  , cdr :: NonEmpty a
  }
  deriving (Eq, Ord, Show)

cadr :: AtLeastTwo a -> a
cadr = head . cdr

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

