module Data.Csv.Decoder where

import Control.Applicative (Applicative (pure, (<*>)), liftA2)
import Data.Csv.Record (Record)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation)

newtype Decode e s a =
  Decode (Record s -> AccValidation e a)

instance Functor (Decode e s) where
  fmap f (Decode g) = Decode (fmap (fmap f) g)

instance Semigroup e => Apply (Decode e s) where
  Decode f <.> Decode a = Decode (liftA2 ((<.>)) f a)

instance Semigroup e => Applicative (Decode e s) where
  pure = Decode . pure . pure
  (<*>) = (<.>)

instance Alt (Decode e s) where
  Decode f <!> Decode g = Decode (liftA2 (<!>) f g)

instance Profunctor (Decode e) where
  dimap f g (Decode d) = Decode (dimap (fmap f) (fmap g) d)

