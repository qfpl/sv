module Text.Between where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (foldMap))
import Data.Functor (Functor (fmap))
import Data.Traversable (Traversable (traverse))

data Between str a =
  Between {
    before :: str
  , after :: str
  , value :: a
  }
  deriving (Eq, Ord, Show)

instance Functor (Between str) where
  fmap f (Between b t a) = Between b t (f a)

instance Foldable (Between str) where
  foldMap f = f . value

instance Traversable (Between str) where
  traverse f (Between b t a) = fmap (Between b t) (f a)

instance Bifunctor Between where
  bimap f g (Between b t a) = Between (f b) (f t) (g a)

betwixt :: s -> a -> s -> Between s a
betwixt b a t = Between b t a

uniform :: s -> a -> Between s a
uniform s a = Between s s a

