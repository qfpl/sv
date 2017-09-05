module Text.Between (
  Between (Between, before, value, after)
  , betwixt
  , uniform
) where

import Data.Bifoldable     (Bifoldable (bifoldMap))
import Data.Bifunctor      (Bifunctor (bimap))
import Data.Bitraversable  (Bitraversable (bitraverse))
import Data.Foldable       (Foldable (foldMap))
import Data.Functor        (Functor (fmap))
import Data.Monoid         ((<>))
import Data.Traversable    (Traversable (traverse))

data Between str a =
  Between {
    before :: str
  , value :: a
  , after :: str
  }
  deriving (Eq, Ord, Show)

instance Functor (Between str) where
  fmap f (Between b a t) = Between b (f a) t

instance Foldable (Between str) where
  foldMap f = f . value

instance Traversable (Between str) where
  traverse f (Between b a t) = fmap (betwixt b t) (f a)

instance Bifunctor Between where
  bimap f g (Between b a t) = Between (f b) (g a) (f t)

instance Bifoldable Between where
  bifoldMap f g (Between b a t) = f b <> g a <> f t

instance Bitraversable Between where
  bitraverse f g (Between b a t) = Between <$> f b <*> g a <*> f t

betwixt :: s -> s -> a-> Between s a
betwixt b t a = Between b a t

uniform :: s -> a -> Between s a
uniform s a = Between s a s

