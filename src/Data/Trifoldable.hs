module Data.Trifoldable where

import Data.Monoid ((<>))

class Trifoldable t where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> t a b c -> m

instance Trifoldable (,,) where
  trifoldMap f g h (a,b,c) = f a <> g b <> h c

