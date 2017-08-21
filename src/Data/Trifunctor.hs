module Data.Trifunctor where

class Trifunctor t where
  trimap :: (a -> x) -> (b -> y) -> (c -> z) -> t a b c -> t x y z

instance Trifunctor (,,) where
  trimap f g h (a,b,c) = (f a, g b, h c)

