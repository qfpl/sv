{-# LANGUAGE RankNTypes #-}

module Data.Functor.Compose.Extra where

import Data.Functor.Compose (Compose (Compose))

lmapC :: (forall z. f z -> f' z) -> Compose f g a -> Compose f' g a
lmapC f (Compose fga) = Compose (f (fga))

rmapC :: Functor f => (g a -> g' a') -> Compose f g a -> Compose f g' a'
rmapC g (Compose fga) =
  Compose (fmap g fga)

injl :: (Functor f, Applicative g) => f a -> Compose f g a
injl fa = Compose (fmap pure fa)

injr :: (Applicative f) => g a -> Compose f g a
injr ga = Compose (pure ga)
