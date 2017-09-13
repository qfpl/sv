{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Something between two other things
module Text.Between (
  Between (Between, _before, _value, _after)
  , HasBetween (between, before, after, value)
  , betwixt
  , empty
  , uniform
) where

import Control.Lens        (Lens')
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
    _before :: s
  , _value :: a
  , _after :: s
  }
  deriving (Eq, Ord, Show)

class HasBetween c s a | c -> s a where
  between :: Lens' c (Between s a)
  after :: Lens' c s
  {-# INLINE after #-}
  before :: Lens' c s
  {-# INLINE before #-}
  value :: Lens' c a
  {-# INLINE value #-}
  after = between . after
  before = between . before
  value = between . value

instance HasBetween (Between s a) s a where
  {-# INLINE after #-}
  {-# INLINE before #-}
  {-# INLINE value #-}
  between = id
  before f (Between x y z) = fmap (\w -> Between w y z) (f x)
  value f (Between x y z) = fmap (\w -> Between x w z) (f y)
  after f (Between x y z) = fmap (Between x y) (f z)

instance Functor (Between s) where
  fmap f (Between b a t) = Between b (f a) t

-- | Appends the right parameter on the inside of the left parameter
--
-- Eg. Between "aaa" () "bbb" *> Between "ccc" () "ddd" == Between "aaaccc" () "dddbbb"
instance Monoid s => Applicative (Between s) where
  pure = empty
  Between b f t <*> Between b' a t' = Between (b <> b') (f a) (t' <> t)

instance Foldable (Between s) where
  foldMap f = f . _value

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

