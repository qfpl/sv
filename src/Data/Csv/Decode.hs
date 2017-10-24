{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Csv.Decode where

import Control.Applicative (Applicative (pure, (<*>)), liftA2)
import Control.Lens (view)
import Data.Csv.Field (Field)
import Data.Csv.Record (Record)
import Data.Functor.Alt (Alt ((<!>)))
import Data.Functor.Apply (Apply ((<.>)))
import Data.Profunctor (Profunctor (dimap))
import Data.Semigroup (Semigroup)
import Data.Validation (AccValidation (AccSuccess, AccFailure), _AccValidation, Validate)

newtype Decode e s a =
  Decode { runDecode :: s -> AccValidation e a }

newtype FieldDecode e s a =
  FieldDecode { runFieldDecode :: Decode e (Field s) a }

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

decoder :: Validate v => (s -> v e a) -> Decode e s a
decoder f = Decode (view _AccValidation . f)

failure :: e -> Decode e s a
failure = decoder . const . AccFailure

success :: a -> Decode e s a
success = decoder . const . AccSuccess

class RunDecode d s a | d -> s a where
  run :: d -> s -> a

instance RunDecode (Decode e s a) s (AccValidation e a) where
  run = runDecode

instance RunDecode (FieldDecode e s a) (Field s) (AccValidation e a) where
  run = run . runFieldDecode

instance RunDecode (RowDecode e s a) (Record s) (AccValidation e a) where
  run = run . runRowDecode

