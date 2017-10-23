module Data.Csv.Decoder where

import Control.Applicative (Applicative (pure, (<*>)), liftA2)
import Data.Csv.Field (Field')
import Data.Csv.Record (Record)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation)

newtype Decode e s a =
  Decode { runDecode :: s -> AccValidation e a }

newtype FieldDecode e s1 s2 a =
  FieldDecode { runFieldDecode :: Decode e (Field' s1 s2) a }

newtype RowDecode e s a =
  RowDecode { runRowDecode :: Decode e (Record s) a }

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
  dimap f g (Decode d) = Decode (dimap f (fmap g) d)

instance Functor (RowDecode e s) where
  fmap f = RowDecode . fmap f . runRowDecode

instance Semigroup e => Apply (RowDecode e s) where
  RowDecode f <.> RowDecode a = RowDecode (f <.> a)

instance Semigroup e => Applicative (RowDecode e s) where
  pure = RowDecode . pure
  (<*>) = (<.>)

instance Alt (RowDecode e s) where
  RowDecode f <!> RowDecode g = RowDecode (f <!> g)

instance Profunctor (RowDecode e) where
  dimap f g (RowDecode d) = RowDecode (dimap (fmap f) g d)

