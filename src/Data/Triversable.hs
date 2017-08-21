module Data.Triversable where

class Triversable t where
  triverse :: Applicative f => (a -> f x) -> (b -> f y) -> (c -> f z) -> t a b c -> f (t x y z)

instance Triversable (,,) where
  triverse f g h (a,b,c) = (,,) <$> f a <*> g b <*> h c

