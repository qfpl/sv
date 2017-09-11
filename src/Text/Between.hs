-- | Something between two other things
module Text.Between (
  Between (Between, before, value, after)
  , betwixt
  , empty
  , uniform
) where

import Data.Bifoldable     (Bifoldable (bifoldMap))
import Data.Bifunctor      (Bifunctor (bimap))
import Data.Bitraversable  (Bitraversable (bitraverse))
import Data.Foldable       (Foldable (foldMap))
import Data.Functor        (Functor (fmap))
import Data.Monoid         ((<>))
import Data.Traversable    (Traversable (traverse))


-- | 'Between s a' is an 'a' between a pair of 's's.

-- This is useful for things like quotes around strings.
data Between s a =
  Between {
    before :: s
  , value :: a
  , after :: s
  }
  deriving (Eq, Ord, Show)

instance Functor (Between s) where
  fmap f (Between b a t) = Between b (f a) t

-- | Appends the right parameter on the inside of the left parameter
--
-- Eg. Between "aaa" () "bbb" *> Between "ccc" () "ddd" == Between "aaaccc" () "dddbbb"
instance Monoid s => Applicative (Between s) where
  pure = empty
  Between b f t <*> Between b' a t' = Between (b <> b') (f a) (t' <> t)

instance Foldable (Between s) where
  foldMap f = f . value

instance Traversable (Between s) where
  traverse f (Between b a t) = fmap (betwixt b t) (f a)

instance Bifunctor Between where
  bimap f g (Between b a t) = Between (f b) (g a) (f t)

instance Bifoldable Between where
  bifoldMap f g (Between b a t) = f b <> g a <> f t

instance Bitraversable Between where
  bitraverse f g (Between b a t) = Between <$> f b <*> g a <*> f t

-- | 'betwixt' is just the constructor for 'Between' with a different
-- argument order, which is sometimes useful.
betwixt :: s -> s -> a-> Between s a
betwixt b t a = Between b a t

-- | 'empty a' is an 'a' that is between two empty 's's.
empty :: Monoid s => a -> Between s a
empty = uniform mempty

-- | `uniform s a` is 'a' between two of the same 's'.
-- Useful for singleton types like '()'
uniform :: s -> a -> Between s a
uniform s a = Between s a s

